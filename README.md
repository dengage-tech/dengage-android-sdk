# Dengage Android SDK

## Table of Contents

- [SDK Setup](#sdk-setup)
  - [Requirements](#requirements)
  - [SDK Installation](#sdk-installation)
  - [FCM Setup](#fcm-setup)
  - [End Point Configuration](#end-point-configuration)
- [Initialization](#initialization)
- [Register Lifecycle Callbacks](#register-lifecycle-callbacks)

## Android SDK Setup

### Requirements

- Minimum Android version supported `19+`

### SDK Installation

Dengage SDK is available on JitPack package repository. If you haven't done so yet, add the JitPack repository in your project's `build.gradle` file:

```groovy
buildscript {
    repositories {
        ...
        maven { url 'https://jitpack.io' }
    }
}
```

The Dengage SDK is organized into three modules, allowing you to import only what you need:

1. **Core SDK**: If you don’t plan to use the geofence feature or Huawei messaging service, simply include the `sdk` module.
2. **Geofence**: To enable geofence functionality, add the `sdk-geofence` module in addition to the `sdk` module.
3. **Huawei Messaging Service**: If you want to use Huawei messaging service, include the `sdk-hms` module along with the `sdk` module.

| **Module**   | **Description**                                                                                  |
| ------------ | ------------------------------------------------------------------------------------------------ |
| sdk          | Core module required for analytics, in-app messaging and Firebase messaging service integration. |
| sdk-geofence | Enables geofence features.                                                                       |
| sdk-hms      | Huawei messaging service integration.                                                            |

Lates SDK version: `6.0.76`

```groovy
dependencies {
    implementation 'com.github.dengage-tech.dengage-android-sdk:sdk:6.0.76'
}
```

### FCM setup

1. Complete the [FCM Android Setup](https://firebase.google.com/docs/android/setup) to configure your Android application for Firebase integration.
2. Download the `google-services.json` configuration file and place it in your app's directory.
3. To ensure the values in your `google-services.json` configuration file are accessible to Firebase SDKs, you need to include the **Google Services Gradle Plugin (`google-services`)** in your project.
   - In your root-level (project-level) Gradle file `<project>/build.gradle`, add the following dependency::
   ```groovy
   dependencies {
       classpath("com.google.gms:google-services:4.4.2")
   }
   ```
   - In your module (app-level) Gradle file `<project>/app/build.gradle`, apply the Google Services plugin as follows:
   ```groovy
   plugins {
       id("com.google.gms.google-services")
   }
   ```
4. To handle push messages, you need to include the **FcmMessagingService** in your `AndroidManifest.xml` file. Place the following block inside the `<application>` tag of your `AndroidManifest.xml` file to ensure proper integration:
    
    ```xml
    <!-- Add the FCM Messaging Service to handle push notifications from Firebase -->
    <service
        android:name="com.dengage.sdk.push.FcmMessagingService"
        android:exported="false" >
        <intent-filter>
            <action android:name="com.google.firebase.MESSAGING_EVENT" />
        </intent-filter>
    </service>
    ```

### Endpoint Configuration

For the initial setup, if you have been provided with URL addresses by the **Dengage Support Team**, you need to configure these URLs in the `AndroidManifest.xml` file.

Refer to the [API Endpoints By Datacenter](https://dev.dengage.com/reference/api-endpoints-by-datacenter) section to correctly set your API endpoint.

Here’s an example configuration:

```xml
<meta-data
    android:name="den_event_api_url"
    android:value="https://your_api_endpoint" />
<meta-data
    android:name="den_push_api_url"
    android:value="https://your_api_endpoint" />
<meta-data
    android:name="den_device_id_api_url"
    android:value="https://your_api_endpoint" />
<meta-data
    android:name="den_in_app_api_url"
    android:value="https://your_api_endpoint" />
<meta-data
    android:name="den_geofence_api_url"
    android:value="https://your_api_endpoint" />
<meta-data
    android:name="fetch_real_time_in_app_api_url"
    android:value="https://your_api_endpoint" />
```

**Note:** Ensure the URLs match the ones provided by the Dengage Support Team and are appropriate for your data center.

## Initialization

### Register Lifecycle Callbacks

To ensure proper functionality, you need to register activity lifecycle callbacks with `DengageLifecycleTracker`. This step is crucial for:

- Fetching specific content from the API, such as the in-app message list.
- Calculating session and visit-related analytics.

You can register the callbacks using the following code in your `Application` class:

```kotlin
registerActivityLifecycleCallbacks(DengageLifecycleTracker())
```
