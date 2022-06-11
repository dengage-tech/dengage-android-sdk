package com.dengage.sdk.manager.geofence

import com.dengage.sdk.domain.geofence.model.GeofenceEvent
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView
import com.dengage.sdk.domain.geofence.model.GeofenceCluster as Cluster

interface GeofenceLocationContract {

    interface View : BaseView {
        fun fetchedGeofenceClusters(clusters: Array<Cluster>?, error: Throwable?)
        fun geofenceEventSignalSent(event: GeofenceEvent?, error: Throwable?)
    }

    interface Presenter : BasePresenter<View> {

        fun getGeofenceClusters(integrationKey: String, lat: Double, lon: Double)

        fun sendGeofenceEventSignal(
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
        )
    }
}