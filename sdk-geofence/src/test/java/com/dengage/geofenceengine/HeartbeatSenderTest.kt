package com.dengage.geofenceengine

import android.location.Location
import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.geofenceengine.storage.SyncMetadataRepository
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.ContextHolder
import io.mockk.coEvery
import io.mockk.coVerify
import io.mockk.every
import io.mockk.mockk
import io.mockk.mockkObject
import io.mockk.unmockkAll
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.joinAll
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import retrofit2.Response
import java.io.IOException

@Config(manifest = Config.NONE)
@RunWith(RobolectricTestRunner::class)
class HeartbeatSenderTest {

    private val metadata = InMemorySyncMetadata()
    private val api = mockk<GeofenceRepository>()
    private val sender = HeartbeatSender(metadata, api)
    private val location = Location("test").apply {
        latitude = 41.0
        longitude = 29.0
    }

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
        mockkObject(Prefs)
        every { Prefs.subscription } returns Subscription(integrationKey = "key", deviceId = "device")
        coEvery { api.sendDeviceHeartbeat(any(), any()) } returns Response.success(Unit)
    }

    @After
    fun tearDown() {
        unmockkAll()
    }

    @Test
    fun `concurrent calls send a single heartbeat`() = runBlocking {
        // Ağ gecikmesi: eski kodda tüm çağrılar bu sürede eski lastHeartbeatAt'i okuyup gönderiyordu.
        coEvery { api.sendDeviceHeartbeat(any(), any()) } coAnswers {
            delay(200)
            Response.success(Unit)
        }

        List(20) { launch(Dispatchers.IO) { sender.maybeSend(location, intervalMinutes = 60) } }.joinAll()

        coVerify(exactly = 1) { api.sendDeviceHeartbeat(any(), any()) }
    }

    @Test
    fun `successful send blocks next call within interval`() = runBlocking {
        sender.maybeSend(location, intervalMinutes = 60)
        sender.maybeSend(location, intervalMinutes = 60)

        coVerify(exactly = 1) { api.sendDeviceHeartbeat(any(), any()) }
    }

    @Test
    fun `http failure restores previous timestamp so next call retries`() = runBlocking {
        val previous = System.currentTimeMillis() - 2 * HOUR_MS
        metadata.lastHeartbeatAt = previous
        coEvery { api.sendDeviceHeartbeat(any(), any()) } returns Response.error(500, "".toResponseBody(null))

        sender.maybeSend(location, intervalMinutes = 60)
        assertEquals(previous, metadata.lastHeartbeatAt)

        coEvery { api.sendDeviceHeartbeat(any(), any()) } returns Response.success(Unit)
        sender.maybeSend(location, intervalMinutes = 60)

        coVerify(exactly = 2) { api.sendDeviceHeartbeat(any(), any()) }
        assertTrue(metadata.lastHeartbeatAt!! > previous)
    }

    @Test
    fun `exception restores previous timestamp`() = runBlocking {
        coEvery { api.sendDeviceHeartbeat(any(), any()) } throws IOException("offline")

        sender.maybeSend(location, intervalMinutes = 60)

        assertNull(metadata.lastHeartbeatAt)
    }

    @Test
    fun `force within floor is skipped`() = runBlocking {
        metadata.lastHeartbeatAt = System.currentTimeMillis() - 5 * MINUTE_MS

        sender.maybeSend(location, intervalMinutes = 60, force = true)

        coVerify(exactly = 0) { api.sendDeviceHeartbeat(any(), any()) }
    }

    @Test
    fun `force after floor sends even within interval`() = runBlocking {
        metadata.lastHeartbeatAt = System.currentTimeMillis() - 16 * MINUTE_MS

        sender.maybeSend(location, intervalMinutes = 60)
        coVerify(exactly = 0) { api.sendDeviceHeartbeat(any(), any()) }

        sender.maybeSend(location, intervalMinutes = 60, force = true)
        coVerify(exactly = 1) { api.sendDeviceHeartbeat(any(), any()) }
    }

    @Test
    fun `no subscription neither sends nor reserves`() = runBlocking {
        every { Prefs.subscription } returns null

        sender.maybeSend(location, intervalMinutes = 60)

        coVerify(exactly = 0) { api.sendDeviceHeartbeat(any(), any()) }
        assertNull(metadata.lastHeartbeatAt)
    }

    private class InMemorySyncMetadata : SyncMetadataRepository {
        override var lastETag: String? = null
        override var lastSyncedAt: Long? = null
        override var lastHeartbeatAt: Long? = null
        override var lastSilentPushAt: Long? = null
        override var wakeupPausedAt: Long? = null
        override var stoppedAt: Long? = null
    }

    private companion object {
        const val MINUTE_MS = 60_000L
        const val HOUR_MS = 60 * MINUTE_MS
    }
}
