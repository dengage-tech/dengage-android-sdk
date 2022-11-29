package com.dengage.sdk.manager.geofence

import com.dengage.sdk.domain.geofence.model.GeofenceEvent
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.domain.geofence.usecase.GetGeofenceClusters
import com.dengage.sdk.domain.geofence.usecase.SendGeofenceEventSignal
import java.util.*

class GeofenceLocationPresenter : BaseAbstractPresenter<GeofenceLocationContract.View>(),
    GeofenceLocationContract.Presenter {

    private val getGeofenceClusters by lazy { GetGeofenceClusters() }
    private val sendGeofenceEventSignal by lazy { SendGeofenceEventSignal() }

    override fun getGeofenceClusters(integrationKey: String, lat: Double, lon: Double) {
        if (isGeofenceeEnabled()) {
            getGeofenceClusters(this) {
                onResponse = {
                    view { fetchedGeofenceClusters(it, null) }
                }
                onError = {
                    view { fetchedGeofenceClusters(null, it) }
                }
                params = GetGeofenceClusters.Params(
                    integrationKey = integrationKey,
                    latitude = lat,
                    longitude = lon
                )
            }
        }
    }

    override fun sendGeofenceEventSignal(
        integrationKey: String,
        identifier: String,
        clusterId: Int,
        geofenceId: Int,
        deviceId: String,
        contactKey: String,
        latitude: Double,
        longitude: Double,
        type: String,
        token: String,
        permit: Boolean
    ) {
        if (isGeofenceeEnabled()) {
            sendGeofenceEventSignal(this) {
                onResponse = {
                    val event = GeofenceEvent (
                        identifier = identifier,
                        cid = clusterId,
                        geoid = geofenceId,
                        type = type,
                        et = Date().time,
                        pp = permit
                    )
                    view { geofenceEventSignalSent( event,null) }
                }
                onError = {
                    view { geofenceEventSignalSent(null, it) }
                }
                params = SendGeofenceEventSignal.Params(
                    integrationKey = integrationKey,
                    clusterId = clusterId,
                    geofenceId = geofenceId,
                    deviceId = deviceId,
                    contactKey = contactKey,
                    latitude = latitude,
                    longitude = longitude,
                    type = type,
                    token = token,
                    permit = permit
                )
            }

        }
    }

    private fun isGeofenceeEnabled(): Boolean {
        return true
    }

}
