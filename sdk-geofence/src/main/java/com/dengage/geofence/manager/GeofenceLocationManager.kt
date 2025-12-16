package com.dengage.geofence.manager

import java.util.*
import android.annotation.SuppressLint as SL
import android.location.Location
import com.dengage.sdk.Dengage
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.LocationRequest
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingRequest as GR
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.geofence.manager.GeofenceLocationContract as GLC
import com.dengage.sdk.util.ContextHolder as CH
import com.dengage.sdk.util.DengageLogger as DL
import com.dengage.geofence.manager.GeofencePermissionsHelper as GPH
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource as GLS
import com.dengage.sdk.domain.geofence.model.GeofenceState as GState
import com.dengage.sdk.domain.geofence.model.GeofenceCluster as Cluster
import com.dengage.sdk.domain.geofence.model.*
import com.dengage.sdk.util.Constants.GEOFENCE_EVENT_HISTORY_MAX_COUNT as GEHMC
import com.dengage.sdk.util.Constants.GEOFENCE_FETCH_HISTORY_MAX_COUNT as GFHMC
import com.dengage.sdk.util.Constants.GEOFENCE_MAX_MONITOR_COUNT
import com.dengage.sdk.util.Constants.GEOFENCE_MAX_EVENT_SIGNAL_INTERVAL_MILISECONDS as GMESIM
import com.dengage.sdk.util.Constants.GEOFENCE_MAX_FETCH_INTERVAL_MILISECONDS as GMFIM
import com.dengage.sdk.util.Constants.SYNCED_GEOFENCES_REQUEST_ID_PREFIX
import com.dengage.sdk.util.Constants.DESIRED_MOVING_UPDATE_INTERVAL as DMUI
import com.dengage.sdk.util.Constants.FASTEST_MOVING_UPDATE_INTERVAL as FMUI
import com.dengage.sdk.util.Constants.DESIRED_SYNC_INTERVAL as DSI
import com.dengage.sdk.util.Constants.STOP_DURATION
import com.dengage.sdk.util.Constants.STOP_DISTANCE
import kotlin.math.*


@SL("MissingPermission")
internal class GeofenceLocationManager : BaseMvpManager<GLC.View, GLC.Presenter>(), GLC.View {

    override fun providePresenter() = GeofenceLocationPresenter()

    @SL("VisibleForTests")
    internal var lClient = LocationServices.getFusedLocationProviderClient(CH.context)

    @SL("VisibleForTests")
    internal var gClient = LocationServices.getGeofencingClient(CH.context)

    private val R = 6372.8 // In kilometers
    private var started = false
    private var startedDesiredAccuracy = LocationRequest.PRIORITY_NO_POWER
    private var startedInterval = 0
    private var startedFastestInterval = 0

    private var gHistory: GeofenceHistory = Prefs.geofenceHistory
    private var sending = false

    init {
        try {
            Dengage.setLocationPermission(status = GPH.getLocationPermissionStatusString(CH.context))
        } catch (e: Exception) {
            DL.error("Error setting location permission status | error = ${e.message}")
        }
        if (Prefs.geofenceEnabled) {
            startTracking()
        }
    }

    fun startTracking() {
        this.stopLocationUpdates()
        if (!GPH.fineLocationPermission(CH.context) && !GPH.coarseLocationPermission(CH.context)) {
            DL.debug("Geofence permissions missing")
            return
        }
        Prefs.geofenceEnabled = true
        this.updateTracking()
        val lastFetchedClusters =
            gHistory.fetchHistory[gHistory.fetchHistory.keys.sortedByDescending { it }
                .firstOrNull()]
        if(lastFetchedClusters != null) {
            replaceSyncedGeofences(lastFetchedClusters)
        }
        GState.getLastLocation()?.let {
            if ((Date().time - GMFIM) > gHistory.lastFetchTime && !sending){
                sending = true
                fetchGeofenceClusters(it)
                return
            }
        }
    }

    fun stopGeofence() {
        this.started = false
        Prefs.geofenceEnabled = false
        this.updateTracking()
    }

    internal fun updateTracking(location: Location? = null) {
        DL.debug("Updating tracking; location = $location")
        if (Prefs.geofenceEnabled) {
            if (GState.stopped) {
                this.stopLocationUpdates()
                if (location != null) {
                    this.replaceBubbleGeofence(location, true)
                }
            } else {
                this.startLocationUpdates()
                if (location != null) {
                    this.replaceBubbleGeofence(location, false)
                } else {
                    this.removeBubbleGeofences()
                }
            }
        } else {
            this.stopLocationUpdates()
            this.removeAllGeofences()
        }
    }

    private fun startLocationUpdates() {
        if (!started || (LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY != startedDesiredAccuracy) || (DMUI != startedInterval) || (FMUI != startedFastestInterval)) {
            val priority = LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY
            val locationRequest = LocationRequest.create().setPriority(priority).setInterval(DMUI * 1000L).setFastestInterval(FMUI * 1000L)
            lClient.requestLocationUpdates(
                locationRequest,
                GeofenceLocationReceiver.getLocationPendingIntent(CH.context)
            )
            this.started = true
            this.startedDesiredAccuracy = LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY
            this.startedInterval = DMUI
            this.startedFastestInterval = FMUI
        }
    }

    private fun stopLocationUpdates() {
        lClient.removeLocationUpdates(GeofenceLocationReceiver.getLocationPendingIntent(CH.context))
        started = false
    }

    internal fun handleBootCompleted() {
        DL.debug("Handling boot completed")
        this.started = false
        GState.stopped = false
        lClient.lastLocation.addOnSuccessListener { location: Location? ->
            updateTracking(location)
        }.addOnFailureListener {
            updateTracking()
        }
    }


    private fun replaceBubbleGeofence(location: Location?, stopped: Boolean = false) {
        if (location == null) {
            return
        }
     //   this.removeBubbleGeofences()
     /*   if (stopped) {
            val identifier = BUBBLE_STOPPED_GEOFENCE_REQUEST_ID
            val radius = STOPPED_GEOFENCE_RADIUS.toFloat()
            val geofence = Geofence.Builder()
                .setRequestId(identifier)
                .setCircularRegion(location.latitude, location.longitude, radius)
                .setExpirationDuration(Geofence.NEVER_EXPIRE)
                .setTransitionTypes(Geofence.GEOFENCE_TRANSITION_EXIT)
                .build()

            val request = GR.Builder().addGeofence(geofence)
                .setInitialTrigger(Geofence.GEOFENCE_TRANSITION_EXIT).build()

            DL.debug("Adding stopped bubble geofence | latitude = ${location.latitude}; longitude = ${location.longitude}; radius = $radius; identifier = $identifier")

            gClient.addGeofences(request, GLR.getBubbleGeofencePendingIntent(CH.context))
                .run {
                    addOnSuccessListener {
                        DL.debug("Successfully added stopped bubble geofence")
                    }
                    addOnFailureListener {
                        DL.debug("Error adding stopped bubble geofence | message = ${it.message}")
                    }
                }
        } else if (!stopped) {
            val identifier = BUBBLE_MOVING_GEOFENCE_REQUEST_ID
            val radius = MOVING_GEOFENCE_RADIUS.toFloat()

            val geofence = Geofence.Builder()
                .setRequestId(identifier)
                .setCircularRegion(location.latitude, location.longitude, radius)
                .setExpirationDuration(Geofence.NEVER_EXPIRE)
                .setLoiteringDelay(STOP_DURATION * 1000 + 10000)
                .setTransitionTypes(Geofence.GEOFENCE_TRANSITION_DWELL or Geofence.GEOFENCE_TRANSITION_EXIT)
                .build()

            val request = GR.Builder()
                .addGeofence(geofence)
                .setInitialTrigger(Geofence.GEOFENCE_TRANSITION_DWELL or Geofence.GEOFENCE_TRANSITION_EXIT)
                .build()

            DL.debug("Adding moving bubble geofence | latitude = ${location.latitude}; longitude = ${location.longitude}; radius = $radius; identifier = $identifier")
            gClient.addGeofences(request, GLR.getBubbleGeofencePendingIntent(CH.context))
                .run {
                    addOnSuccessListener {
                        DL.debug("Successfully added moving bubble geofence")
                    }
                    addOnFailureListener {
                        DL.debug("Error adding moving bubble geofence | message = ${it.message}")
                    }
                }
        }*/
    }

    private fun replaceSyncedGeofences(geofenceClusters: Array<Cluster>?) {
        this.removeSyncedGeofences()
        if (geofenceClusters == null) {
            return
        }
        val geofenceItems = mutableMapOf<String, Cluster.GeofenceItem>()
        geofenceClusters.forEach { c ->
            c.geofences.forEach { g ->
                geofenceItems["${SYNCED_GEOFENCES_REQUEST_ID_PREFIX}_${c.id}_${g.id}"] = g
            }
        }
        val lastLocation = GState.getLastLocation()
        val validGeofenceItems = geofenceItems.toList().sortedBy { (_, v) ->
            var distance = 0.0
            if (lastLocation != null) {
                distance = haversine(lastLocation.latitude, lastLocation.longitude, v.latitude, v.longitude)
            }
            distance
        }.take(GEOFENCE_MAX_MONITOR_COUNT).toMap()

        val validGeofences = mutableListOf<Geofence>()

        for(validGeofenceItem in validGeofenceItems) {
            val identifier = validGeofenceItem.key
            val lat = validGeofenceItem.value.latitude
            val lon = validGeofenceItem.value.longitude
            val radius = validGeofenceItem.value.radius.toFloat()
            try {
                val geofence = Geofence.Builder()
                    .setRequestId(identifier)
                    .setCircularRegion(lat, lon, radius)
                    .setExpirationDuration(Geofence.NEVER_EXPIRE)
                    .setLoiteringDelay(STOP_DURATION * 1000 + 10000)
                    .setTransitionTypes(Geofence.GEOFENCE_TRANSITION_ENTER)
                    .build()
                validGeofences.add(geofence)
                DL.debug("Adding synced geofence | latitude = $lat; longitude = $lon; radius = $radius; identifier = $identifier")
            } catch (e: Exception) {
                DL.debug("Error building synced geofence | latitude = $lat; longitude = $lon; radius = $radius; error = ${e.message}")
            }
        }


        if (validGeofences.isEmpty()) {
            DL.debug("No synced geofences")
            return
        }


        val request = GR.Builder().addGeofences(validGeofences).setInitialTrigger(0).build()

        gClient.addGeofences(request, GeofenceLocationReceiver.getSyncedGeofencesPendingIntent(CH.context))
            .run {
                addOnSuccessListener {
                    DL.debug("Successfully added synced geofences")
                }
                addOnFailureListener {
                    DL.debug("Error adding synced geofences | message = ${it.message}")
                }
            }
    }

    private fun removeBubbleGeofences() {
        gClient.removeGeofences(GeofenceLocationReceiver.getBubbleGeofencePendingIntent(CH.context))
        DL.debug("Removed bubble geofences")
    }

    private fun removeSyncedGeofences() {
        gClient.removeGeofences(GeofenceLocationReceiver.getSyncedGeofencesPendingIntent(CH.context))
        DL.debug("Removed synced geofences")
    }

    private fun removeAllGeofences() {
        this.removeBubbleGeofences()
        this.removeSyncedGeofences()
    }

    fun handleLocation(location: Location?, source: GLS, geofenceRequestId: String?) {
        DL.debug("Handling location | location = $location")
        if (location == null || !GState.valid(location)) {
            DL.debug("Invalid location | source = $source; location = $location")
            return
        }
        val wasStopped = GState.stopped
        var stopped: Boolean
        val force = (source == GLS.FOREGROUND_LOCATION || source == GLS.MANUAL_LOCATION)
        if (!force && location.accuracy > 1000) {
            DL.debug("Skipping location: inaccurate | accuracy = ${location.accuracy}")
            this.updateTracking(location)
            return
        }
        //val duration: Long
        if (STOP_DISTANCE > 0 && STOP_DURATION > 0) {
            var lastMovedLocation = GState.getLastMovedLocation()
            if (lastMovedLocation == null) {
                lastMovedLocation = location
                GState.setLastMovedLocation(lastMovedLocation)
            }
            //var lastMovedAt = GState.lastMovedAt
            //if (lastMovedAt == 0L) {
            //    lastMovedAt = location.time
            //    GState.lastMovedAt = lastMovedAt
            //}
            //if (!force && lastMovedAt > location.time) {
                //DL.debug("Skipping location: old | lastMovedAt = $lastMovedAt; location.time = ${location.time}")
                //return
            //}
            val distance = location.distanceTo(lastMovedLocation)
            //duration = (location.time - lastMovedAt) / 1000
            //stopped = (distance < STOP_DISTANCE && duration > STOP_DURATION)
            stopped = (distance < STOP_DISTANCE)
            //DL.debug("Calculating stopped | stopped = $stopped; distance = $distance; duration = $duration; location.time = ${location.time}; lastMovedAt = $lastMovedAt")
            DL.debug("Calculating stopped | stopped = $stopped; distance = $distance; location.time = ${location.time}")
            if (distance > STOP_DISTANCE) {
                GState.setLastMovedLocation(location)
                if (!stopped) {
                    GState.lastMovedAt = location.time
                }
            }
        } else {
            stopped = force || source == GLS.GEOFENCE_DWELL
        }
        val justStopped = stopped && !wasStopped
        GState.stopped = stopped
        GState.setLastLocation(location)
        if (source != GLS.MANUAL_LOCATION) {
            this.updateTracking(location)
        }
        var sendLocation = location

        val lastFailedStoppedLocation = GState.getLastFailedStoppedLocation()
        var replayed = false
        if (lastFailedStoppedLocation != null && !justStopped) {
            sendLocation = lastFailedStoppedLocation; stopped = true; replayed = true
            GState.setLastFailedStoppedLocation(null)
            DL.debug("Replaying location | location = $location; stopped = $stopped")
        }
        val lastSentAt = GState.lastSentAt
        // Geofence events should bypass rate limiting as they are important events
        val isGeofenceEvent = source == GLS.GEOFENCE_ENTER || source == GLS.GEOFENCE_EXIT || source == GLS.GEOFENCE_DWELL
        val ignoreSync = lastSentAt == 0L || justStopped || replayed || isGeofenceEvent
        val now = System.currentTimeMillis()
        val lastSyncInterval = (now - lastSentAt) / 1000L
        if (!ignoreSync) {
            if (lastSyncInterval < DSI) {
                DL.debug("Skipping sync: desired sync interval | desiredSyncInterval = ${DSI}; lastSyncInterval = $lastSyncInterval")
                return
            }
            if (!force && !justStopped && lastSyncInterval < 1) {
                DL.debug("Skipping sync: rate limit | justStopped = $justStopped; lastSyncInterval = $lastSyncInterval")
                return
            }
        } else if (isGeofenceEvent) {
            DL.debug("Bypassing rate limit for geofence event | source = $source")
        }
        GState.lastSentAt = System.currentTimeMillis()
        if (source == GLS.FOREGROUND_LOCATION) return
        this.sendLocation(sendLocation, stopped, source, replayed, geofenceRequestId)
    }

    private fun fetchGeofenceClusters(location: Location) {
        presenter.getGeofenceClusters(
            integrationKey = Prefs.subscription!!.integrationKey,
            lat = location.latitude,
            lon = location.longitude
        )
    }

    override fun fetchedGeofenceClusters(clusters: Array<Cluster>?, error: Throwable?) {
        if (error != null || clusters == null) {
            DL.error("Error fetching geofences | error = $error")
        } else {
            synchronized(GeofenceLocationManager::class.java) {
                val now = Date()
                gHistory.lastFetchTime = now.time
                GState.getLastLocation()?.let {
                    gHistory.lastLat = it.latitude
                    gHistory.lastLon = it.longitude
                }
                if (!gHistory.fetchHistory.containsKey(now.time)) {
                    gHistory.fetchHistory.set(now.time, clusters)
                }
                replaceSyncedGeofences(clusters)
                updateGeofenceHistory()
            }
        }
        updateTracking(GState.getLastLocation())
        sending = false
    }

    fun sendGeofenceEventSignal(
        identifier: String,
        clusterId: Int,
        geofenceId: Int,
        type: String,
        latitude: Double,
        longitude: Double
    ) {
        presenter.sendGeofenceEventSignal(
            integrationKey = Prefs.subscription!!.integrationKey,
            identifier = identifier,
            clusterId = clusterId,
            geofenceId = geofenceId,
            deviceId = Prefs.subscription!!.deviceId ?: "",
            contactKey = Prefs.subscription!!.deviceId ?: "",
            latitude = latitude,
            longitude = longitude,
            type = type,
            token = Prefs.subscription!!.token ?: "",
            permit = Prefs.subscription!!.permission ?: true
        )
    }

    override fun geofenceEventSignalSent(event: GeofenceEvent?, error: Throwable?) {
        if (error != null || event == null) {
            DL.error("Error sending geofence event signal | error = $error")
        } else {
            synchronized(GeofenceLocationManager::class.java) {
                if (!gHistory.eventHistory.containsKey(event.identifier)) {
                    gHistory.eventHistory.set(event.identifier, hashMapOf())
                }
                gHistory.eventHistory[event.identifier]?.set(event.et, event)
                GState.getLastLocation()?.let {
                    gHistory.lastLat = it.latitude
                    gHistory.lastLon = it.longitude
                }
                updateGeofenceHistory()
            }
        }
        updateTracking(GState.getLastLocation())
        sending = false
    }

    private fun sendLocation(
        location: Location,
        stopped: Boolean,
        source: GLS,
        replayed: Boolean,
        geofenceRequestId: String?
    ) {
        if (sending) {
            DL.debug("Sending location: Already sending, skipping")
            return
        }
        sending = true
        DL.debug("Sending location | source = $source; location = $location; stopped = $stopped; replayed = $replayed; geofenceRequestId = $geofenceRequestId")

        if (source == GLS.GEOFENCE_ENTER || source == GLS.GEOFENCE_EXIT) {
            DL.debug("Geofence event detected | source = $source")
            if (geofenceRequestId?.startsWith(SYNCED_GEOFENCES_REQUEST_ID_PREFIX) == true) {
                DL.debug("Geofence requestId is valid | requestId = $geofenceRequestId")
                val geofenceRequestIdArr = geofenceRequestId.split("_").toTypedArray()
                DL.debug("Geofence requestId split | parts = ${geofenceRequestIdArr.joinToString()}")
                if (geofenceRequestIdArr.count() >= 4) {
                    val clusterId = geofenceRequestIdArr[2].toIntOrNull()
                    val geofenceId = geofenceRequestIdArr[3].toIntOrNull()
                    DL.debug("Extracted IDs | clusterId = $clusterId; geofenceId = $geofenceId")
                    val lastFetchedClusters =
                        gHistory.fetchHistory[gHistory.fetchHistory.keys.sortedByDescending { it }
                            .firstOrNull()]
                    DL.debug("Last fetched clusters | available = ${lastFetchedClusters != null}; fetchHistory keys = ${gHistory.fetchHistory.keys.size}")
                    if (clusterId != null && geofenceId != null && lastFetchedClusters != null) {
                        val cluster = lastFetchedClusters.firstOrNull { it.id == clusterId }
                        DL.debug("Cluster lookup | clusterId = $clusterId; found = ${cluster != null}")
                        val fetchedGeofence =
                            cluster?.geofences?.firstOrNull { it.id == geofenceId }
                        DL.debug("Geofence lookup | geofenceId = $geofenceId; found = ${fetchedGeofence != null}")
                        val events = gHistory.eventHistory[geofenceRequestId]
                        DL.debug("Event history | requestId = $geofenceRequestId; hasHistory = ${events != null}; historySize = ${events?.size ?: 0}")
                        if (fetchedGeofence != null) {
                            val type = if (source == GLS.GEOFENCE_ENTER) "enter" else "exit"
                            if (events != null) {
                                val lastEvent =
                                    events[events.keys.sortedByDescending { it }.firstOrNull()]
                                val currentTime = Date().time
                                val timeSinceLastEvent = if (lastEvent != null) (currentTime - lastEvent.et) else Long.MAX_VALUE
                                DL.debug("Event time check | lastEvent = $lastEvent; currentTime = $currentTime; timeSinceLastEvent = $timeSinceLastEvent; requiredInterval = $GMESIM")
                                if (lastEvent == null || (currentTime - GMESIM) > lastEvent.et ) {
                                    DL.debug("Sending geofence event signal | type = $type; clusterId = $clusterId; geofenceId = $geofenceId")
                                    sendGeofenceEventSignal(
                                        geofenceRequestId,
                                        clusterId,
                                        geofenceId,
                                        type,
                                        location.latitude,
                                        location.longitude
                                    )
                                    return
                                } else {
                                    DL.debug("Skipping geofence event signal: Rate limit | timeSinceLastEvent = $timeSinceLastEvent; requiredInterval = $GMESIM")
                                }
                            } else {
                                DL.debug("Sending geofence event signal (first event) | type = $type; clusterId = $clusterId; geofenceId = $geofenceId")
                                sendGeofenceEventSignal(
                                    geofenceRequestId,
                                    clusterId,
                                    geofenceId,
                                    type,
                                    location.latitude,
                                    location.longitude
                                )
                                return
                            }
                        } else {
                            DL.debug("Skipping geofence event signal: Geofence not found in fetched clusters | geofenceId = $geofenceId; clusterId = $clusterId")
                        }
                    } else {
                        DL.debug("Skipping geofence event signal: Missing data | clusterId = $clusterId; geofenceId = $geofenceId; lastFetchedClusters = ${lastFetchedClusters != null}")
                    }
                } else {
                    DL.debug("Skipping geofence event signal: Invalid requestId format | parts count = ${geofenceRequestIdArr.count()}; required = 4")
                }
            } else {
                DL.debug("Skipping geofence event signal: RequestId doesn't start with prefix | requestId = $geofenceRequestId; prefix = $SYNCED_GEOFENCES_REQUEST_ID_PREFIX")
            }
            sending = false
        } else {
            DL.debug("Not a geofence event | source = $source")
            if ((Date().time - GMFIM) > gHistory.lastFetchTime){
                DL.debug("Fetching geofence clusters | lastFetchTime = ${gHistory.lastFetchTime}")
                fetchGeofenceClusters(location)
                return
            } else {
                DL.debug("Skipping geofence fetch: Rate limit | lastFetchTime = ${gHistory.lastFetchTime}")
            }
        }
        sending = false
    }

    private fun updateGeofenceHistory() {
        if (gHistory.fetchHistory.count() > GFHMC) {
            val keysToBeDeleted = gHistory.fetchHistory.keys.sortedBy { it }
                .take(gHistory.fetchHistory.count() - GFHMC)
            keysToBeDeleted.forEach { gHistory.fetchHistory.remove(it) }
        }
        val timeIdDic = mutableMapOf<Long, String>()
        for (idEventDic in gHistory.eventHistory) {
            for (idEvent in idEventDic.value) {
                timeIdDic.put(idEvent.key, idEventDic.key)
            }
        }
        if (timeIdDic.count() > GEHMC) {
            val keysToBeDeleted = timeIdDic.keys.sortedBy { it }.take(timeIdDic.count() - GEHMC)
            for (key in keysToBeDeleted) {
                gHistory.eventHistory.remove(timeIdDic[key])
            }
        }
        Prefs.geofenceHistory = gHistory
    }

    fun haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
        var locLat1 = lat1
        var locLat2 = lat2
        val dLat = Math.toRadians(locLat2 - locLat1)
        val dLon = Math.toRadians(lon2 - lon1)
        locLat1 = Math.toRadians(locLat1)
        locLat2 = Math.toRadians(locLat2)
        val a = sin(dLat / 2).pow(2.0) + sin(dLon / 2).pow(2.0) * cos(locLat1) * cos(locLat2)
        val c = 2 * asin(sqrt(a))
        return R * c
    }

}