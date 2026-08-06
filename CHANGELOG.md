# Changelog

## [6.0.99] - 2026-08-06

### Bug Fixes

- Do not call `finish()` immediately after `showRatingDialog` in `InAppMessageActivity`; defer closing the in-app so Play In-App Review can hand off to the underlying activity

## [6.0.98] - 2026-08-03

### New Features

- Suppress in-app requests while the app is in the background: silent push, geofence and live-update wake-ups no longer fetch in-app messages, refresh SDK parameters or burn the fetch interval while nobody is on screen
- Track foreground state from the SDK's own activity lifecycle callbacks instead of process importance, which reports "foreground" while a high-priority FCM message is being handled with no activity on screen
- Always fetch in-app messages when the app comes to the foreground, regardless of the fetch interval, with a small floor to absorb accidental background / foreground churn
- Replace the fixed hourly fetch timer with an adaptive per-channel gate that backs off on empty responses and returns to the account interval as soon as a message arrives, capped at 15 minutes
- Fetch SDK parameters once per process when the first activity is shown, instead of during `Dengage.init`
- Skip the fetch interval and the minimum time between messages in development mode, which now also covers devices listed in `debugDeviceIds`, so test devices see campaigns immediately
- Add `SessionManager.currentSessionId` for read-only session access that never rotates the session

### Bug Fixes

- Open the session when an activity is started rather than in `Dengage.init`, so a silent push waking the process no longer starts a session and inflates `dn.visit_count`
- Slide the session expiry on user activity instead of only setting it when a new session starts, so an active user is no longer rotated to a new session every 30 minutes
- Stop rule evaluation and debug logging from rotating the session as a side effect of reading the session id
- Fix `Prefs.appSessionId` returning a freshly generated UUID on every read when no session was stored, since the default value was never persisted
- Prevent duplicate in-app requests when a second trigger arrives while a request is still in flight
- Reschedule the next fetch after a failed request instead of leaving the window consumed


## [6.0.97] - 2026-07-29

### Bug Fixes

- Use the app's push small icon for offline geofence notifications instead of a generic system icon

## [6.0.96] - 2026-07-24

### New Features

- Introduce Geofence Engine v2 (`com.dengage.geofenceengine`): server-synced geofences with ETag revalidation, nearest-N monitoring, and enter / exit / dwell reporting through `POST /event-signal` v2
- Keep `DengageGeofence` as the public entry point; `startGeofence` / `stopGeofence` / `requestLocationPermissions` now drive the new engine
- Handle geofence silent push (`sourceType=geofence`) automatically in `FcmMessagingService` via `GeofenceSilentPushDispatcher`, with a reflection bridge so the core SDK works without the geofence module
- Emit synthetic transitions when the OS callback is late or missing, so a dropped exit no longer leaves a fence permanently stuck
- Repair missed exits on other fences when an OS transition wakes the engine, without firing campaigns for them
- Gate synthetic transitions on fix confidence: both horizontal accuracy and fix age feed the decision margin, and fixes older than 15 minutes are ignored
- Send `occurredAt` from the location fix time instead of processing time, and report `accuracyM`, `syntheticTransition` and `token` in event-signal requests
- Cache campaign content for offline triggers and fire it as a local notification, queueing the event until connectivity returns
- Add remote tuning through `SdkParameters.geofence` (top-N, re-evaluation distance, adaptive threshold, wake-up cap, heartbeat interval, offline queue size), clamped to safe ranges on read
- Disable FusedLocationProvider batching so location updates arrive without delay

### Bug Fixes

- Keep the process alive with `goAsync()` while geofence events are being sent, fixing push notifications arriving hours after the trigger
- Deduplicate OS transitions so a single physical crossing no longer produces multiple events
- Persist the wake-up cap pause so it survives process restarts instead of silently re-enabling location updates
- Re-register geofences after a location-provider toggle or app update, which can clear OS-side registrations
- Send `occurredAt` and `ingestedAt` as UTC ISO-8601 with a `Z` suffix, matching the iOS SDK
- Fix `LocalNotificationFirer` for empty deep links

### Documentation

- Document Geofence Engine v2 integration, permissions, silent push and radius/accuracy guidance


## [6.0.95] - 2026-06-30

### New Features

- Add `hideIfNotFound` support to `Dengage.showStoriesList` and `Dengage.showInAppInline` to automatically hide placements when no matching campaign is found
- Sync cancelled in-app campaigns via the `getCancelledSendIds` API and remove them from the local cache after each in-app fetch
- Hide status bar in fullscreen inapp

### Bug Fixes

- Clear `StoriesListView` content (title and story list) when the targeted story campaign is unavailable
- Hide App Story and In-App Inline containers when `hideIfNotFound` is `true` and property id / targeting does not match

### Improvements

- Replace the deprecated `ExpiredInAppMessageRequest` flow with `getCancelledSendIds` for more reliable in-app message cache cleanup

## [6.0.93] - 2026-06-22

### New Features

- Implement getCancelledSendIds

## [6.0.94] - 2026-06-22

### New Features

- Add `hideIfNotFound` support for App Story via `Dengage.showStoriesList`
- Clear embedded `StoriesListView` content when no matching story campaign is found

### Bug Fixes

- Hide the story placement when `hideIfNotFound` is `true` and property id / targeting does not match


## [6.0.92] - 2026-06-03

### New Features

- Implement Live Updates for ongoing-activity notifications (delivery tracking, live scores, etc.)
- Route `live_notification` FCM data messages through `DengageLiveUpdateManager` with START / UPDATE / END lifecycle validation
- Register per-activity-type notification handlers via `DengageLiveUpdateManager.register`
- Manage the full notification lifecycle inside the SDK (channel creation, posting, cancellation, and `dismissalDate` auto-dismissal); handlers only implement `buildNotification`
- Add new features to App Story

### Documentation

- Add Live Updates section to readme


## [6.0.91] - 2026-05-09

### New Features

- Implement GeofenceInterceptor
- Include system push authorization status in subscription permission

## [6.0.90] - 2026-03-31

### Bug Fixes

- Hide story CTA button when isEnabled is false

## [6.0.89] - 2026-03-27

### New Features

- Add user-controlled event tracking permission
- Add absoluteEndDate control for COUNTDOWN_TO_WIN inapp notifications
- Enable edge-to-edge in InAppMessageActivity

### Bug Fixes

- Hide inline element when in-app not found


## [6.0.88] - 2026-02-20

### Bug Fixes

- Prevent duplicate push open events using messageDetails deduplication
- Fix HttpRequestHandler response body
- Remove notifications enabled check in subscription request


## [6.0.87] - 2026-01-22

### New Features
- Implement `subscriptionEnabled` switch in SdkParameters
- Implement `eventsEnabled` switch in SdkParameters
- Implement `geofenceEnabled` switch in SdkParameters
- Implement `copyToClipboard` functionality in In-App Messages
- Implement DengageBridge for in-app messages
- Implement campaign variables for DengageBridge
- Add API URLs to HttpRequestHandler in DengageBridge

### Bug Fixes
- Fix duplicate in-app message display and multiple coupon assignments on consecutive `setNavigation` calls
- Remove RuntimeException in `setDomain` in ConfigurationManager

### Improvements
- Persist in-app message showCount across API responses with auto-cleanup
- Improve geofence event handling and logging

### Documentation
- Update geofence section in readme
