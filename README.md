# Dengage Android SDK

## Table of Contents

- [SDK Setup](#sdk-setup)
  - [Requirements](#requirements)
  - [SDK Installation](#sdk-installation)
  - [Endpoint Configuration](#endpoint-configuration)
- [Integration](#integration)
  - [Register Lifecycle Callbacks](#register-lifecycle-callbacks)
  - [Initialization](#initialization)
  - [Logging](#logging)
- [User Profiles](#user-profiles)
  - [setContactKey](#setcontactkey)
  - [setDeviceId](#setdeviceid)
  - [setCountry](#setcountry)
- [User Events](#user-events)
  - [Login](#login)
    - [setContactKey](#setcontactkey)
    - [getSubscription](#getsubscription)
  - [eCommerce Events](#ecommerce-events)
    - [Page View Events](#page-view-events)
    - [Shopping Cart Events](#shopping-cart-events)
    - [Order Events](#order-events)
    - [Search Event](#search-event)
    - [Wishlist Events](#wishlist-events)
  - [Custom Events](#custom-events)
- [Push Notifications](#push-notifications)
  - [FCM Setup](#fcm-setup)
  - [Subscription](#subscription)
  - [User Permission Management](#user-permission-management-optional)
  - [Event Tracking Permission](#event-tracking-permission-optional)
  - [Notification Channels](#notification-channels)
  - [getToken](#gettoken)
  - [Action Buttons](#action-buttons)
  - [Carousel Push](#carousel-push)
    - [Defining Custom Receiver](#defining-custom-receiver)
    - [Preparing Custom Layouts](#preparing-custom-layouts)
    - [Building Carousel Notification](#building-carousel-notification)
- [Live Updates](#live-updates)
  - [How It Works](#how-it-works)
  - [Implementing a Handler](#implementing-a-handler)
  - [Registering a Handler](#registering-a-handler)
  - [Payload Reference](#payload-reference)
- [App Inbox](#app-inbox)
  - [Methods](#methods)
    - [Getting Inbox Messages](#getting-inbox-messages)
    - [Removing an Inbox Message](#removing-an-inbox-message)
    - [Removing all Inbox Messages](#removing-all-inbox-messages)
    - [Marking an Inbox Message as Read](#marking-an-inbox-message-as-read)
    - [Marking all Inbox Messages as Read](#marking-all-inbox-messages-as-read)
- [In-App Messaging](#in-app-messaging)
  - [Methods](#methods)
  - [Real Time In-App Messaging](#real-time-in-app-messaging)
  - [Custom Device Information for In-App Messages](#custom-device-information-for-in-app-messages)
  - [In-App Inline](#in-app-inline)
  - [App Stories](#app-stories)
- [Geofence](#geofence)
  - [Geofence Installation](#geofence-installation)
  - [Geofence Initialization](#geofence-initialization)
  - [Request Location Permission](#request-location-permission)
  - [Geofence Interceptor](#geofence-interceptor)
  - [Performance Considerations and Best Practices](#performance-considerations-and-best-practices)
- [Huawei Messaging Service](#huawei-messaging-service)
  - [HMS setup](#hms-setup)
  - [Initialization for Huawei](#initialization-for-huawei)


## SDK Setup

### Requirements

- Minimum Android version supported `19+`

### SDK Installation

Dengage SDK is available on JitPack package repository. If you haven't done so yet, add the JitPack repository in your project's `build.gradle` file:

```groovy
buildscript {
    repositories {
        // ... other repositories
        maven { url 'https://jitpack.io' }
    }
}
```

The Dengage SDK is organized into three modules, allowing you to import only what you need:

1. **Core SDK**: If you don’t plan to use the geofence feature or Huawei messaging service, simply include the `sdk` module.
2. **Geofence**: To enable geofence functionality, add the `sdk-geofence` module in addition to the `sdk` module.
3. **Huawei Messaging Service**: If you want to use Huawei messaging service, include the `sdk-hms` module along with the `sdk` module.

| **Module**   | **Description**                                                                                  |
|--------------|--------------------------------------------------------------------------------------------------|
| sdk          | Core module required for analytics, in-app messaging and Firebase messaging service integration. |
| sdk-geofence | Enables geofence features.                                                                       |
| sdk-hms      | Huawei messaging service integration.                                                            |

Latest SDK version: `6.0.93`

```groovy
dependencies {
    implementation 'com.github.dengage-tech.dengage-android-sdk:sdk:6.0.93'
}
```

### Endpoint Configuration

For the initial setup, if you have been provided with URL addresses by the **Dengage Support Team**, you need to configure these URLs in the `AndroidManifest.xml` file.

Refer to the [API Endpoints By Datacenter](https://dev.dengage.com/reference/api-endpoints-by-datacenter) section to correctly set your API endpoints.

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

## Integration

### Register Lifecycle Callbacks

To ensure the proper functionality of the Dengage SDK, you need to register activity lifecycle callbacks with `DengageLifecycleTracker`. This step is crucial for:

- Fetching specific content from the API, such as the in-app message list.
- Calculating session and visit-related analytics.

You can register the callbacks using the following code in your `Application` class:

```kotlin
registerActivityLifecycleCallbacks(DengageLifecycleTracker())
```

### Initialization

`init` method should be called once during the application's lifecycle to initialize the Dengage SDK. It is recommended to place it inside the `onCreate` method of your `Application` class.

```kotlin
Dengage.init(
    context = applicationContext,
    firebaseIntegrationKey = "your-firebase-integration-key",
    deviceConfigurationPreference = DeviceConfigurationPreference.Google,
    disableOpenWebUrl = false
)
```

| **Parameter Name**                | **Description**                                                                                                                        |
|-----------------------------------|----------------------------------------------------------------------------------------------------------------------------------------|
| **context**                       | The application context used for local cache operations.                                                                               |
| **firebaseIntegrationKey**        | The integration key generated by the CDMP Platform while defining the application. It is a hash string containing application details. |
| **deviceConfigurationPreference** | Specifies the configuration preference. For Google Play applications, it should be set to `Google`.                                    |
| **disableOpenWebUrl**             | Set this parameter to `true` if you want to prevent the browser from opening when a push notification is clicked.                      |

If you need to update the Dengage Firebase app integration key after the SDK has been initialized, call `setFirebaseIntegrationKey` with the new key.

```kotlin
Dengage.setFirebaseIntegrationKey(
    integrationKey = "your-firebase-integration-key"
)
```

### Logging

You can enable or disable logs comes from Dengage sdk to your **Logcat**.

```kotlin
Dengage.setLogStatus(enable = true)
```

## User Profiles

### setContactKey

The **Contact Key** serves as a bridge between **Devices** and **Contacts**. Devices can be categorized into two types:

1. **Anonymous Devices**
2. **Contact Devices** (which include a Contact Key)

To associate devices with their respective contacts, the **Contact Key** must be set in the SDK.

> **Recommended Usage:**  
You should call this method if you have user information.  
It is recommended to call this method during every app launch, as well as on login and logout pages.

```kotlin
Dengage.setContactKey(contactKey = "contact-key")
```

### setDeviceId

You can set a unique device id of for current device. This id will be used to identify the device in the Dengage system.

```kotlin
Dengage.setDeviceId(deviceId = "unique-identifier-of-device")
```

### setCountry

You can set country of current user subscription.

```kotlin
Dengage.setCountry(country = "country-code")
```




## User Events


In order to collect app events and use that data to create behavioral segments in Dengage you have to determine the type of events and data that needs to collect. Once you have determined that, you will need to create a “Big Data” table in Dengage. Collected events will be stored in this table. Multiple tables can be defined depending on your specific need.

Any type of event can be collected. The content and the structure of the events are completely flexible and can be changed according to unique business requirements. You will just need to define a table for events.

Once defined, all you have to do is to send the event data to these tables. Dengage SDK has only 2 functions for sending events: `sendDeviceEvent` and `sendCustomEvent`. Most of the time you will just need the sendDeviceEvent function.

For eCommerce accounts, there are predefined event tables. And you can feed these tables by using eCommerce event functions.

### Login

If the user logs in or you have user information, this means you have contact_key for that user. You can set contact_key in order to match user with the device. There are two functions for getting and setting contact_key.

#### setContactKey

If user logged in set user id. This is important for identifying your users. You can put this function call in every page. It will not send unnecessary events.

```kotlin
Dengage.setContactKey(contactKey = "contact-key")
```

#### getSubscription

If you need to get current user information from SDK use this function. `contactKey` is a property of `Subscription` object.

```kotlin
Dengage.getSubscription()
```

### eCommerce Events

If your Dengage account is an eCommerce account, you should use standard eCommerce events in the SDK. If you need some custom events or your account is not a standard eCommerce account, you should use custom event functions.

Dengage SDK includes standard eCommerce events:

* **Page View Events**:
  * Home page view
  * Product page view
  * Category page view
  * Promotion page view
  * ...
* **Shopping Cart Events**:
  * Add to cart
  * Remove from cart
  * View cart
  * Begin checkout
* **Order Events**:
  * Order
  * Cancel order
* **Wishlist Events**:
  * Add to wishlist
  * Remove from wishlist
* **Search Event**

Each event corresponds to related tables in your account.

#### Page View Events

Page view events are sent to the `page_view_events` table. If you've added new columns to this table, include them in the event data.

```kotlin
// Home page view
val data = hashMapOf<String, Any>(
    "page_type" to "home"
    // ... additional columns in page_view_events table can be added here
)
Dengage.pageView(data)

// Product page view
val data = hashMapOf<String, Any>(
    "page_type" to "product",
    "product_id" to 123
    // ... additional columns in page_view_events table can be added here
)
Dengage.pageView(data)

//Category page view
val data = hashMapOf<String, Any>(
    "page_type" to "category",
    "category_id" to 123
    // ... additional columns in page_view_events table can be added here
)
Dengage.pageView(data)

// Promotion page view
val data = hashMapOf<String, Any>(
    "page_type" to "promotion",
    "promotion_id" to 123
    // ... additional columns in page_view_events table can be added here
)
Dengage.pageView(data)

// Custom page view
val data = hashMapOf<String, Any>(
    "page_type" to "custom"
    // ... additional columns in page_view_events table can be added here
)
Dengage.pageView(data)
```

> For other pages you can send anything as page_type

#### Shopping Cart Events

These events are stored in `shopping_cart_events` and `shopping_cart_events_detail` tables. The following functions are available for shopping cart events:

1. `addToCart`
2. `removeFromCart`
3. `viewCart`
4. `beginCheckout`


```kotlin
// Add To Cart
val data = hashMapOf<String, Any>(
    "product_id" to "123",
    "product_variant_id" to "1222",
    "quantity" to 1,
    "unit_price" to 9.99,
    "shipping" to 5,
    "discounted_price" to 9.99,
    "coupon_code" to ""
    // ... extra columns in shopping_cart_events table, can be added here
)
Dengage.addToCart(data)

// Remove From Cart
val data = hashMapOf<String, Any>(
    "product_id" to "123",
    "product_variant_id" to "1222",
    "quantity" to 1,
    "unit_price" to 9.99,
    "shipping" to 5,
    "discounted_price" to 9.99,
    "coupon_code" to ""
    // ... extra columns in shopping_cart_events table, can be added here
)
Dengage.removeFromCart(data)

// View Cart
val data = hashMapOf<String, Any>(
    // ... extra columns in shopping_cart_events table, can be added here
)
Dengage.viewCart(data)


// Begin Checkout
val data = hashMapOf<String, Any>(
    // ... extra columns in shopping_cart_events table, can be added here
)
Dengage.beginCheckout(data)
```

#### Order Events

Order events are stored in `order_events` and `order_events_detail` tables.

```kotlin
// Paid Order
val data = hashMapOf<String, Any>(
    "order_id" to "123",
    "item_count" to 1,
    "total_amount" to 99.9,
    "payment_method" to "card",
    "shipping" to 5,
    "discounted_price" to 29.99, // use total price if there is no discount
    "coupon_code" to "" // use if necessary
    // ... extra columns in order_events table, can be added here
)
Dengage.order(data)

// Cancel Order
val data = hashMapOf<String, Any>(
    "item_count" to 1,
    "total_amount" to 99.9,
    "discounted_price" to 29.99 // use total price if there is no discount
    // ... extra columns in order_events table, can be added here
)
Dengage.cancelOrder(data)
```

#### Search Event

Search events are stored in the `search_events` table.

```kotlin
val data = hashMapOf<String, Any>(
    "keywords" to "hello",
    "result_count" to 123,
    "filters" to "q=keywords" // you can send extra filters selected by the user here
    // ... extra columns in search_events table, can be added here
)
Dengage.search(data)
```

#### Wishlist Events

These events are stored in `wishlist_events` and `wishlist_events_detail` tables. The available functions are:

1. `addToWishlist`
2. `removeFromWishlist`

You can send all items in the wishlist for every event, simplifying the tracking of the current wishlist items.

```kotlin
// Add To Wishlist
val data = hashMapOf<String, Any>(
    "product_id" to 123
    // ... extra columns in wishlist_events table can be added here
)
Dengage.addToWishList(data)

// Remove From Wishlist
val data = hashMapOf<String, Any>(
    "product_id" to 123
    // ... extra columns in wishlist_events table can be added here
)
Dengage.removeFromWishList(data)
```

### Custom Events

Use the `sendDeviceEvent` function to send events specific to a device. Events are sent to a big data table defined in your D·engage account, which must have a relation to the `master_device` table. If you set a `contact_key` for that device, collected events will be associated with the user.

```kotlin
// For example, if you have a table named "events"
// and the events table has "key", "event_date", "event_name", "product_id" columns
// You only need to send the columns except "key" and "event_date", as those are sent by the SDK

val data = hashMapOf<String, Any>(
    "event_name" to "page_view",
    "product_id" to "12345"
    // ... extra columns in events table, can be added here
)
Dengage.sendDeviceEvent("events", data)

```




## Push Notifications


### FCM setup

1. Complete the [FCM Android Setup](https://firebase.google.com/docs/android/setup) to configure your Android application for Firebase integration.
2. Download the `google-services.json` configuration file and place it in your app's directory.
3. To ensure the values in your `google-services.json` configuration file are accessible to Firebase SDKs, you need to include the **Google Services Gradle Plugin (`google-services`)** in your project.
- In your root-level (project-level) Gradle file `<project>/build.gradle`, add the following dependency:
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

### Subscription

`Subscription` is self-managed by SDK.

> **Definition**: Subscription is a process which is triggered by sending subscription event to Dengage. It contains necessary information about application to send push notifications to clients.

The SDK automatically sends subscription events in the following scenarios:

1. Initialization
2. Setting Contact Key
3. Setting Token
4. Setting User Permission (if permissions are manually managed)

### Asking User Permission for Notification

To request notification permission from the user (required for Android 13+), you can use the following method provided by the SDK:

```kotlin
Dengage.requestNotificationPermission(activity)
```

This function checks if the notification permission (`POST_NOTIFICATIONS`) is granted. If not, it will prompt the user to allow notifications. Make sure to call this method from your `Activity` (for example, during onboarding or before sending notifications).

> **Note:** On Android 13 (API level 33) and above, notification permission is required at runtime. On earlier versions, this call will have no effect.


### User Permission Management (optional)

If you manage your own user permission states on your application you may send user permission by using `setUserPermission` method.

```kotlin
// Use to set permission of current subscription
Dengage.setUserPermission(permission = true)

// Use to get permission of current subscription
Dengage.getUserPermission() // Boolean?
```

### Event Tracking Permission (optional)

If you need to let the user enable/disable **event tracking** (analytics events sent by the SDK), you can control it using `setTrackingPermission`.

- **Default**: `true` (events are allowed unless you disable it)
- **Behavior**: when `false`, the SDK will skip sending events even if the backend `eventsEnabled` parameter is `true`
- **Scope**: stored locally (persists across app launches)

```kotlin
// Disable event tracking (user opted out)
Dengage.setTrackingPermission(permission = false)

// Enable event tracking (user opted in)
Dengage.setTrackingPermission(permission = true)
```

### Notification Channels

Starting with **Android O**, every notification must be assigned a valid notification channel; otherwise, it will not be displayed.

The SDK automatically assigns a default channel named **"General"**. This default channel can be customized using the `setNotificationChannelName` method provided by the SDK.

To override the default channel, call the `setNotificationChannelName` method with your desired channel name.

```kotlin
Dengage.setNotificationChannelName("your-custom-channel-name")
```

### getToken

Retrieve the token for the current user's subscription using this method.

```kotlin
val token = Dengage.getToken()
```

### Action Buttons

Dengage Android SDK allows you to add clickable buttons under notifications.

If you need to handle action buttons manually, you must define a custom receiver in your `AndroidManifest.xml` that extends `com.dengage.sdk.NotificationReceiver`. Otherwise, the SDK will handle button clicks automatically.

```xml
<receiver android:name=".MyReceiver"
    android:exported="false"> <!-- Replace NotificationReceiver with .MyReceiver -->
    <intent-filter>
        ...
        <action android:name="com.dengage.push.intent.ACTION_CLICK" /> <!-- Add this line -->
    </intent-filter>
</receiver>
```

To handle intents fired by the SDK, use the following function in your launcher activity:

```kotlin
PushDataParser.parseIntent(intent, pushMessageCallback = object : PushMessageCallback {
    override fun dataFetched(message: Message) {
        // Handle fetched data
    }

    override fun onActionClick(intent: Intent, message: Message, clickedId: String) {
        // Handle action button click
    }
})
```

### Carousel Push

Carousel Push functionality allows you to show your notification with a slideshow.

#### Defining Custom Receiver

Before you start, you need to define your receiver in your `AndroidManifest.xml` file.

```xml
<!-- For handling push notifications comes to messaging service classes -->
<receiver
    android:name=".push.PushNotificationReceiver"
    android:exported="false" >
    <intent-filter>
        <action android:name="com.dengage.push.intent.RECEIVE" />
        <action android:name="com.dengage.push.intent.OPEN" />
        <action android:name="com.dengage.push.intent.DELETE" />
        <action android:name="com.dengage.push.intent.ACTION_CLICK" />
        <action android:name="com.dengage.push.intent.ITEM_CLICK" />
        <action android:name="com.dengage.push.intent.CAROUSEL_ITEM_CLICK" />
    </intent-filter>
</receiver>
```

#### Preparing Custom Layouts

The SDK utilizes custom layouts for carousel functionality. You need to set up these layouts in your `res` folder. Pre-built layouts are available for both landscape and portrait orientations.

* **Landscape Layout:** Add `_den_carousel_landscape.xml` to your `layouts` directory.
* **Portrait Layout:** Add `den_carousel_portrait.xml` to your `layouts` directory.

**Note:** Ensure you replace placeholder drawable items with your own resources.

`den_carousel_landscape.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="wrap_content">

    <include
        android:id="@+id/den_carousel_collapsed"
        layout="@layout/den_carousel_collapsed"
        android:layout_width="match_parent"
        android:layout_height="wrap_content" />

    <FrameLayout
        android:id="@+id/den_carousel_frame"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_collapsed"
        android:layout_gravity="center"
        android:paddingLeft="16dp"
        android:paddingRight="16dp">

        <ImageView
            android:id="@+id/den_carousel_landscape_image"
            android:layout_width="match_parent"
            android:layout_height="160dp"
            android:layout_gravity="center"
            android:scaleType="centerCrop" />

        <ImageView
            android:id="@+id/den_carousel_left_image"
            android:layout_width="44dp"
            android:layout_height="44dp"
            android:layout_gravity="start|center_vertical"
            android:layout_marginStart="5dp"
            android:layout_marginLeft="5dp"
            android:scaleType="fitXY"
            android:src="@drawable/ic_arrow_left" />

        <ImageView
            android:id="@+id/den_carousel_right_image"
            android:layout_width="44dp"
            android:layout_height="44dp"
            android:layout_gravity="end|center_vertical"
            android:layout_marginEnd="5dp"
            android:scaleType="fitXY"
            android:src="@drawable/ic_arrow_right" />

    </FrameLayout>

    <LinearLayout
        android:id="@+id/den_carousel_item_title_container"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_frame"
        android:layout_marginTop="5dp"
        android:orientation="vertical">

        <TextView
            android:id="@+id/den_carousel_item_title"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:gravity="center"
            android:text="Item Title" />

    </LinearLayout>

    <LinearLayout
        android:id="@+id/den_carousel_item_description_container"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_item_title_container"
        android:layout_marginTop="5dp"
        android:layout_marginBottom="5dp"
        android:orientation="vertical">

        <TextView
            android:id="@+id/den_carousel_item_description"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:gravity="center"
            android:text="Item Description" />

    </LinearLayout>

</RelativeLayout>
```

`den_carousel_portrait.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="wrap_content">

    <include
        android:id="@+id/den_carousel_collapsed"
        layout="@layout/den_carousel_collapsed"
        android:layout_width="match_parent"
        android:layout_height="wrap_content" />

    <FrameLayout
        android:id="@+id/den_carousel_body_portrait"
        android:layout_width="596dp"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_collapsed"
        android:layout_gravity="center"
        android:layout_marginBottom="8dp"
        android:paddingLeft="16dp"
        android:paddingRight="16dp">

        <RelativeLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_gravity="center">

            <RelativeLayout
                android:id="@+id/den_carousel_left_container"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:layout_toStartOf="@+id/den_carousel_portrait_current_container"
                android:layout_toLeftOf="@+id/den_carousel_portrait_current_container">

                <ImageView
                    android:id="@+id/den_carousel_portrait_left_image"
                    android:layout_width="192dp"
                    android:layout_height="130dp"
                    android:layout_centerInParent="true"
                    android:scaleType="centerCrop" />

                <RelativeLayout
                    android:layout_width="wrap_content"
                    android:layout_height="wrap_content"
                    android:layout_alignStart="@+id/den_carousel_portrait_left_image"
                    android:layout_alignLeft="@+id/den_carousel_portrait_left_image"
                    android:layout_alignTop="@+id/den_carousel_portrait_left_image"
                    android:layout_alignEnd="@+id/den_carousel_portrait_left_image"
                    android:layout_alignRight="@+id/den_carousel_portrait_left_image"
                    android:layout_alignBottom="@+id/den_carousel_portrait_left_image"
                    android:background="#BFffffff" />
            </RelativeLayout>

            <RelativeLayout
                android:id="@+id/den_carousel_right_container"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:layout_toEndOf="@+id/den_carousel_portrait_current_container"
                android:layout_toRightOf="@+id/den_carousel_portrait_current_container">

                <ImageView
                    android:id="@+id/den_carousel_portrait_right_image"
                    android:layout_width="192dp"
                    android:layout_height="130dp"
                    android:layout_centerInParent="true"
                    android:scaleType="centerCrop" />

                <RelativeLayout
                    android:layout_width="wrap_content"
                    android:layout_height="wrap_content"
                    android:layout_alignStart="@+id/den_carousel_portrait_right_image"
                    android:layout_alignLeft="@+id/den_carousel_portrait_right_image"
                    android:layout_alignTop="@+id/den_carousel_portrait_right_image"
                    android:layout_alignEnd="@+id/den_carousel_portrait_right_image"
                    android:layout_alignRight="@+id/den_carousel_portrait_right_image"
                    android:layout_alignBottom="@+id/den_carousel_portrait_right_image"
                    android:background="#B0ffffff" />

            </RelativeLayout>

            <RelativeLayout
                android:id="@+id/den_carousel_portrait_current_container"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:layout_centerInParent="true">

                <ImageView
                    android:id="@+id/den_carousel_portrait_current_image"
                    android:layout_width="212dp"
                    android:layout_height="130dp"
                    android:cropToPadding="true"
                    android:paddingLeft="10dp"
                    android:paddingRight="10dp"
                    android:scaleType="centerCrop" />

            </RelativeLayout>

        </RelativeLayout>

        <FrameLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_gravity="center"
            android:layout_marginLeft="5dp"
            android:layout_marginRight="5dp">

            <ImageView
                android:id="@+id/den_carousel_left_arrow"
                android:layout_width="44dp"
                android:layout_height="44dp"
                android:layout_gravity="start|center_vertical"
                android:scaleType="fitXY"
                android:src="@drawable/ic_arrow_left" />

            <ImageView
                android:id="@+id/den_carousel_right_arrow"
                android:layout_width="44dp"
                android:layout_height="44dp"
                android:layout_gravity="end|center_vertical"
                android:scaleType="fitXY"
                android:src="@drawable/ic_arrow_right" />

        </FrameLayout>

    </FrameLayout>

    <LinearLayout
        android:id="@+id/den_carousel_item_title_container"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_body_portrait"
        android:orientation="vertical">

        <TextView
            android:id="@+id/den_carousel_item_title"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:gravity="center"
            android:text="Item Title"
            android:textColor="@color/purple_200" />

    </LinearLayout>

    <LinearLayout
        android:id="@+id/den_carousel_item_description_container"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_item_title_container"
        android:layout_marginBottom="8dp"
        android:orientation="vertical">

        <TextView
            android:id="@+id/den_carousel_item_description"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:gravity="center"
            android:text="Item Description"
            android:textColor="@color/purple_200" />

    </LinearLayout>

</RelativeLayout>
```

`den_carousel_collapsed.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:paddingLeft="16dp"
    android:paddingTop="16dp"
    android:paddingRight="16dp">

    <ImageView
        android:id="@+id/den_carousel_image"
        android:layout_width="36dp"
        android:layout_height="36dp"
        android:layout_alignParentEnd="true"
        android:layout_alignParentRight="true"
        android:layout_gravity="top|end"
        android:layout_marginStart="12dp"
        android:layout_marginLeft="12dp"
        android:scaleType="centerCrop"
        android:src="@drawable/ic_launcher_foreground" /> <!-- Your App Icon -->

    <TextView
        android:id="@+id/den_carousel_title"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:layout_toStartOf="@+id/den_carousel_image"
        android:layout_toLeftOf="@id/den_carousel_image"
        android:text="Remote Title"
        android:textColor="@color/purple_200"
        android:textSize="15dp"
        android:textStyle="bold" />

    <TextView
        android:id="@+id/den_carousel_message"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_below="@id/den_carousel_title"
        android:layout_alignStart="@+id/den_carousel_title"
        android:layout_alignLeft="@+id/den_carousel_title"
        android:layout_alignEnd="@+id/den_carousel_title"
        android:layout_alignRight="@+id/den_carousel_title"
        android:layout_marginBottom="16dp"
        android:layout_toLeftOf="@id/den_carousel_image"
        android:ellipsize="end"
        android:lineSpacingMultiplier="1.2"
        android:maxLines="1"
        android:text="Remote message"
        android:textColor="@color/purple_200"
        android:textSize="14dp" />

</RelativeLayout>
```

#### Building Carousel Notification

To use a custom layout for the notification requires building a message by the developer with the layout.

First, please create your receiver class extends from `NotificationReceiver` and override the method `onCarouselRender`. This method will be called when a carousel push notification is received.

```kotlin
class PushNotificationReceiver : NotificationReceiver() {

    override fun onCarouselRender(
        context: Context,
        intent: Intent,
        message: Message,
        leftCarouselItem: CarouselItem,
        currentCarouselItem: CarouselItem,
        rightCarouselItem: CarouselItem
    ) {
        super.onCarouselRender(
            context,
            intent,
            message,
            leftCarouselItem,
            currentCarouselItem,
            rightCarouselItem
        )

        val itemTitle = currentCarouselItem.title
        val itemDesc = currentCarouselItem.description

        // set intents (right button, left button, item click)
        val itemIntent = getItemClickIntent(intent.extras, context.packageName)
        val leftIntent = getLeftItemIntent(intent.extras, context.packageName)
        val rightIntent = getRightItemIntent(intent.extras, context.packageName)
        val deleteIntent = getDeleteIntent(intent.extras, context.packageName)
        val contentIntent = getContentIntent(intent.extras, context.packageName)

        val carouseItemIntent = getPendingIntent(context, 0, itemIntent)
        val carouselLeftIntent = getCarouselDirectionIntent(context, 1, leftIntent)
        val carouselRightIntent = getCarouselDirectionIntent(context, 2, rightIntent)
        val deletePendingIntent = getDeletePendingIntent(context, 4, deleteIntent)
        val contentPendingIntent = getPendingIntent(context, 5, contentIntent)

        // set views for the layout
        val collapsedView = RemoteViews(
            context.packageName,
            R.layout.den_carousel_collapsed
        )
        collapsedView.setTextViewText(R.id.den_carousel_title, message.title)
        collapsedView.setTextViewText(R.id.den_carousel_message, message.message)

        val carouselView = RemoteViews(
            context.packageName,
            R.layout.den_carousel_portrait
        )
        carouselView.setTextViewText(R.id.den_carousel_title, message.title)
        carouselView.setTextViewText(R.id.den_carousel_message, message.message)
        carouselView.setTextViewText(R.id.den_carousel_item_title, itemTitle)
        carouselView.setTextViewText(R.id.den_carousel_item_description, itemDesc)

        carouselView.setOnClickPendingIntent(R.id.den_carousel_left_arrow, carouselLeftIntent)
        carouselView.setOnClickPendingIntent(
            R.id.den_carousel_portrait_current_image,
            carouseItemIntent
        )
        carouselView.setOnClickPendingIntent(R.id.den_carousel_item_title, carouseItemIntent)
        carouselView.setOnClickPendingIntent(R.id.den_carousel_item_description, carouseItemIntent)
        carouselView.setOnClickPendingIntent(R.id.den_carousel_right_arrow, carouselRightIntent)

        val channelId = createNotificationChannel(context, message)

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_left_image,
            carouselItem = leftCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_current_image,
            carouselItem = currentCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_right_image,
            carouselItem = rightCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        val notification = NotificationCompat.Builder(context, channelId)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setCustomContentView(collapsedView)
            .setCustomBigContentView(carouselView)
            .setContentIntent(contentPendingIntent)
            .setDeleteIntent(deletePendingIntent)
            .setSound(message.sound.getSoundUri(context))
            .build()

        // show message again silently with next, previous and current item.
        notification.flags = Notification.FLAG_AUTO_CANCEL or Notification.FLAG_ONLY_ALERT_ONCE

        // show message
        val notificationManager = NotificationManagerCompat.from(context)
        intent.extras?.getInt("requestCode")?.let { notificationManager.notify(it, notification) }
    }
}
```

## Live Updates

Live Updates let you show a single, continuously updating notification for an ongoing activity — such as a delivery in progress, a live match score, or a ride status. Updates are delivered as silent FCM data messages carrying a `live_notification` payload, and the SDK routes each event to a handler you register for that activity type.

### How It Works

The SDK provides the **framework**; your app provides the **notification design**. No FCM wiring is required on your side — the SDK's messaging service automatically detects `live_notification` data messages and dispatches them.

```
FCM data message (live_notification)
        │
        ▼
DengageLiveUpdateManager   ──►  parse payload + validate lifecycle (START / UPDATE / END)
        │
        ▼
your LiveUpdateHandler.buildNotification(context, payload)
        │
        ▼
SDK posts / updates / dismisses the notification
```

The SDK owns the full notification lifecycle — channel creation, posting, cancellation, and auto-dismissal (including `dismissalDate` scheduling). Your handler only builds the notification's visual content.

The event lifecycle is validated automatically:

- **START** — registers the activity and shows the notification.
- **UPDATE** — refreshes the notification, but only if the activity was already started.
- **END** — shows the final state (if any content is provided), then dismisses the notification.

### Implementing a Handler

Implement `LiveUpdateHandler` and override `buildNotification`, plus the channel id/name to post on. Return `null` from `buildNotification` if there is nothing to display for a given payload.

`buildNotification` is the single entry point the SDK calls. Inside it you parse the `content_state` and build the notification however you like. A common pattern is to branch on the platform version — using the native progress notification API on Android 16 (API 36) and above, and a custom `RemoteViews` layout on older devices.

The full example below tracks a delivery order through four steps:

```kotlin
class DeliveryLiveUpdateHandler : LiveUpdateHandler {

    override val channelId = "den_live_update_delivery"
    override val channelName = "Delivery"
    override val channelDescription = "Delivery tracking notifications"

    enum class DeliveryStatus(val label: String, val step: Int) {
        ORDER_RECEIVED("Order received", 1),
        PREPARING("Preparing", 2),
        ON_THE_WAY("On the way", 3),
        DELIVERED("Delivered", 4)
    }

    // The SDK calls this for every START / UPDATE / END event.
    override fun buildNotification(context: Context, payload: LiveUpdatePayload): Notification? {
        val cs = payload.contentState
        val statusName = cs["delivery_status"] ?: return null
        val status = runCatching { DeliveryStatus.valueOf(statusName) }.getOrNull() ?: return null
        val orderId = cs["order_id"] ?: ""
        val eta = cs["estimated_time"] ?: ""

        val notificationId = payload.activityId.hashCode()

        return if (Build.VERSION.SDK_INT >= 36) {
            buildApi36(context, orderId, status, eta, notificationId)
        } else {
            buildLegacy(context, orderId, status, eta, notificationId)
        }
    }

    // Android 16+ (API 36): native ProgressStyle with segments and points.
    @RequiresApi(36)
    private fun buildApi36(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int
    ): Notification {
        val step = status.step
        val isDelivered = status == DeliveryStatus.DELIVERED

        val segments = (1..4).map { i ->
            Notification.ProgressStyle.Segment(1).apply {
                setColor(if (i <= step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }
        val points = (1..3).map { i ->
            Notification.ProgressStyle.Point(i).apply {
                setColor(if (i < step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }
        val progressStyle = Notification.ProgressStyle()
            .setProgress(step)
            .setProgressSegments(segments)
            .setProgressPoints(points)

        val subtitle = if (isDelivered) "Order delivered"
        else "${status.label} · ETA: $eta"

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager

        return Notification.Builder(context, channelId)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle("Order #$orderId")
            .setContentText(subtitle)
            .setStyle(progressStyle)
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setVisibility(Notification.VISIBILITY_PUBLIC)
            .setCategory(Notification.CATEGORY_PROGRESS)
            .setContentIntent(mainIntent(context, notificationId))
            .setShortCriticalText(if (isDelivered) "Delivered" else status.label)
            .apply {
                // Promote to an ongoing live notification when permitted.
                if (!isDelivered && nm.canPostPromotedNotifications()) {
                    addExtras(Bundle().apply {
                        putBoolean("android.requestPromotedOngoing", true)
                    })
                }
            }
            .build()
    }

    // Android < 16: custom RemoteViews layout for the expanded notification.
    private fun buildLegacy(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int
    ): Notification {
        val isDelivered = status == DeliveryStatus.DELIVERED

        val expandedView = RemoteViews(context.packageName, R.layout.notification_live_delivery).apply {
            setTextViewText(R.id.tvLiveDeliveryOrderId, "#$orderId")
            setTextViewText(R.id.tvLiveDeliveryStatus, status.label)
            setTextViewText(R.id.tvLiveDeliveryEta, eta)
            setProgressBar(R.id.progressLiveDelivery, 4, status.step, false)
            setViewVisibility(R.id.pbLiveDeliverySpinner, if (isDelivered) View.GONE else View.VISIBLE)
        }

        return NotificationCompat.Builder(context, channelId)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Order Tracking")
            .setContentText(status.label)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
            .setCategory(NotificationCompat.CATEGORY_PROGRESS)
            .setContentIntent(mainIntent(context, notificationId))
            .build()
    }

    private fun mainIntent(context: Context, requestCode: Int): PendingIntent {
        val intent = Intent(context, MainActivity::class.java)
        return PendingIntent.getActivity(context, requestCode, intent, PendingIntent.FLAG_IMMUTABLE)
    }
}
```

**What the handler does:**

- **`buildNotification`** — parses `content_state` into a typed `DeliveryStatus`, returns `null` for unknown/missing status (the SDK then skips posting), and dispatches to the right builder based on the platform version. The `notificationId` is derived from `activityId` so that every update targets the same notification.
- **`buildApi36`** — uses the native `Notification.ProgressStyle` (Android 16+) to render the four delivery steps as colored segments and points, and requests promotion to an ongoing live notification via `requestPromotedOngoing` when `canPostPromotedNotifications()` allows it.
- **`buildLegacy`** — renders a custom `RemoteViews` layout with a progress bar for devices below Android 16.

> **Note**: You only build the visual content. Do **not** call `notify`, `cancel`, create channels, or schedule dismissals — the SDK handles all of that, including auto-dismissal via `dismissalDate`. Advanced hosts that need full control may override `onUpdate` directly instead of implementing `buildNotification`.

### Registering a Handler

Register one handler per activity type — typically in your `Application.onCreate()`, after initializing the SDK. The type string must match the `activity_type` sent in the payload.

```kotlin
override fun onCreate() {
    super.onCreate()

    Dengage.init(
        context = applicationContext,
        firebaseIntegrationKey = "your-firebase-integration-key"
    )

    DengageLiveUpdateManager.register("delivery", DeliveryLiveUpdateHandler())
    DengageLiveUpdateManager.register("sports", SportsLiveUpdateHandler())
}
```

Additional manager methods:

| Method                          | Description                                                        |
|---------------------------------|--------------------------------------------------------------------|
| `register(type, handler)`       | Registers a handler for an activity type (replaces any existing).  |
| `unregister(type)`              | Removes the handler for the given type.                            |
| `isRegistered(type): Boolean`   | Whether a handler is registered for the type.                     |
| `isActive(activityId): Boolean` | Whether the given activity currently has an active session.       |

### Payload Reference

Each event is delivered as a JSON object in the `live_notification` data key:

```json
{
  "activity_type": "delivery",
  "event": "update",
  "activityId": "90db7e2a-5839-53cd-605f-9d3ffc328e21",
  "dismissal_date": 1774888005,
  "content_state": {
    "order_id": "DNG-8821",
    "delivery_status": "ON_THE_WAY",
    "estimated_time": "10 min"
  }
}
```

The SDK parses this into a `LiveUpdatePayload` passed to your handler:

| Field           | Type                  | Description                                                                                  |
|-----------------|-----------------------|----------------------------------------------------------------------------------------------|
| `activityType`  | `String`              | Activity type used to resolve the handler (`activity_type`).                                  |
| `event`         | `LiveUpdateEvent`     | Lifecycle event: `START`, `UPDATE`, or `END`.                                                |
| `activityId`    | `String`              | Unique id of the activity; the same id updates the same notification.                        |
| `contentState`  | `Map<String, String>` | Key-value content from `content_state`, used to render the notification.                     |
| `dismissalDate` | `Long?`               | Optional epoch-seconds timestamp; when set, the SDK auto-dismisses the notification at that time. |

## App Inbox

App Inbox is a screen within a mobile app that stores persistent messages. It's kind of like an email inbox, but it lives inside the app itself. App Inbox differs from other mobile channels such as push notifications or in-app messages. For both push and in-app messages, they're gone once you open them.

In other words, Dengage admin panel lets you keep selected messages on the platform and Mobile SDK may receive and display these messages when needed.

In order to save messages into App Inbox, you need to select the "Save to Inbox" option when sending messages in D·engage the admin panel by assigning an expiration date to it.

After selecting your Push content, you must activate the "Save To Inbox" option.

> To use the app inbox feature, please send an email to tech@dengage.com.

Inbox messages are kept in the memory storage of the phone until the app is completely closed or for a while and Dengage SDK provides functions for getting and managing these messages.

### Methods

When a push message is received with the `addToInbox` parameter, the message is saved during the notification building stage, allowing users to access these messages later. The following methods facilitate interaction with these messages:

#### Getting Inbox Messages

Retrieve stored inbox messages with pagination:

```kotlin
Dengage.getInboxMessages(
    limit = 20, // Number of messages to retrieve
    offset = 0, // Starting point for retrieval
    dengageCallback = object : DengageCallback<MutableList<InboxMessage>> { // Callback for handling retrieved messages
        override fun onResult(result: MutableList<InboxMessage>) {
            // Handle the result
            Toast.makeText(context, "Inbox Messages: ${result.size}", Toast.LENGTH_SHORT)
                .show()
        }

        override fun onError(error: DengageError) {
            // Handle the error
            Toast.makeText(context, "Error: ${error.errorMessage}", Toast.LENGTH_SHORT)
                .show()
        }
    })
```
#### Removing an Inbox Message

Delete a specific inbox message:

```kotlin
Dengage.deleteInboxMessage(
  messageId = "message-id" // ID of the message to delete
)
```

#### Removing all Inbox Messages

Delete all inbox messages:

```kotlin
Dengage.deleteAllInboxMessages()
```

#### Marking an Inbox Message as Read

Mark a message as clicked to update its status:

```kotlin
Dengage.setInboxMessageAsClicked(
  messageId = "message-id" // ID of the message to mark as clicked
)
```

#### Marking all Inbox Messages as Read

Mark all messages as clicked to update their status:

```kotlin
Dengage.setAllInboxMessagesAsClicked()
```

> `receiveDate` property is used to store inbox message receive date. It keeps date as a UTC time format ("yyyy-MM-ddTHH:mm:ss.fffZ"). The applications which are using our SDKs need to convert this UTC date to the client time zone if the applications want to display the message receive date to their users.


## In-App Messaging

An in-app message is a type of mobile message where the notification is displayed within the app. It is not sent at a specific time but it is shown to users when the user is using the app.

Examples include popups, yes/no prompts, banners, and more.

In order to show in-app messages, there is no permit requirement.

### Methods

Created messages will be stored in Dengage backend and will be served to mobile SDKs.

If you integrated mobile SDK correctly for push messages, for using in-app features you just have to add `setNavigation` function to every page navigation.

If you want to use a screen name filter, you should send the screen name to `setNavigation` function in every page navigation.

You should pass the current activity to setNavigation function.

```kotlin
// Without screen filter
Dengage.setNavigation(
  activity = activity as AppCompatActivity // For showing ui of in app message
)

// With screen filter
Dengage.setNavigation(
  activity = activity as AppCompatActivity, // For showing ui of in app message
  screenName = "screen-name" // For filtering in app messages with respect to current screen in your app
)
```

#### removeInAppMessageDisplay

If you want to immediately dismiss / stop the current in-app message display cycle (for example when you open a full-screen flow or you don’t want the current in-app to keep showing), call:

```kotlin
Dengage.removeInAppMessageDisplay()
```

### Real Time In-App Messaging

You can use the real time in-app functionality by using the function.

```kotlin
val customParams = hashMapOf<String, String>()
Dengage.showRealTimeInApp(
  activity = activity as AppCompatActivity, // For showing ui of in app message
  screenName = "screen-name", // For filtering in app messages with respect to current screen in your app(optional)
  params = customParams // For filtering in app messages with respect to custom parameters(optional)
)

// Set cart for using in real time in app comparisons
val cart = Cart(listOf(
  CartItem(
    productId = "product123",
    productVariantId = "variant456",
    categoryPath = "Electronics/Phones",
    price = 999,
    discountedPrice = 799,
    hasDiscount = true,
    hasPromotion = false,
    quantity = 2,
    attributes = mapOf("color" to "black", "storage" to "128GB")
  )
))
Dengage.setCart(cart = cart)

// Set category path for using in real time in app comparisons
Dengage.setCategoryPath(path = "category-path")

// Set cart item count for using in real time in app comparisons
Dengage.setCartItemCount(count = "cart-item-count")

// Set cart amount for using in real time in app comparisons
Dengage.setCartAmount(amount = "cart-amount")

// Set state for using in real time in app comparisons
Dengage.setState(name = "state-name")

// Set city for using in real time in app comparisons
Dengage.setCity(name = "city-name")
```

### Custom Device Information for In-App Messages

You can set custom device information that will be available in your in-app message templates using Mustache templating.

```kotlin
// Set custom device information
Dengage.setInAppDeviceInfo("user_level", "premium")
Dengage.setInAppDeviceInfo("theme", "dark")

// Clear all custom device information
Dengage.clearInAppDeviceInfo()
```

The custom device information you set will be accessible in your in-app message HTML content through the `dnInAppDeviceInfo` object. For example, in your in-app message template, you can use:

```html
<div>Welcome {{#dnInAppDeviceInfo.user_level}}{{.}}{{/dnInAppDeviceInfo.user_level}} user!</div>
<div>You are using {{#dnInAppDeviceInfo.theme}}{{.}}{{/dnInAppDeviceInfo.theme}} app theme</div>
```

### In-App Inline

The **In-App Inline** feature allows you to seamlessly integrate inline in-app messages into your app's content, dynamically populating specific parts of your app for a better user experience.

Define the layout in your XML file to include the `InAppInlineElement` view, which will display the inline content.

`InAppInlineElement`: The placeholder view where the inline content will be displayed.


```xml
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">

  <LinearLayout
          android:layout_width="match_parent"
          android:layout_height="match_parent"
          android:layout_margin="20dp"
          android:orientation="vertical">

    <com.dengage.sdk.ui.inappmessage.InAppInlineElement
            android:id="@+id/inapp_inline_element"
            android:layout_width="match_parent"
            android:layout_height="200dp"
            android:layout_marginTop="12dp" />
  </LinearLayout>

</layout>
```

In your fragment or activity, set up the inline in-app messaging using the `Dengage.showInlineInApp` method.

```kotlin
class InAppInLineFragment : BaseDataBindingFragment<FragmentInappInlineBinding>() {

  override fun getLayoutRes(): Int {
    return R.layout.fragment_inapp_inline
  }

  override fun init() {
    val customParams = hashMapOf<String, String>()
    Dengage.showInlineInApp(screenName = "screen-name",
      inAppInlineElement = binding.inappInlineElement,
      propertyId = "property-id",
      activity = requireActivity(),
      customParams = customParams,
      hideIfNotFound = true
    )
  }

}
```

Parameters:

* **`inAppInlineElement`** The `InAppInlineElement` view defined in the XML layout where the inline content will appear.
* **`propertyId`** The Android selector linked to the in-app inline campaign created in the Dengage panel.
* **`customParams`** _(optional)_ A `HashMap` of custom parameters used for filtering inline messages.
* **`screenName`** _(optional)_ Specifies the screen where the inline in-app message will be displayed.
* **`hideIfNotFound`** _(optional, default: `false`)_ If set to `true`, the `InAppInlineElement` will be hidden if no inline message is found.



### App Stories

The **App Stories** feature allows you to display story-like content within your app.

Define the layout in your XML file to include the `StoriesListView` view, which will display the story content.

`StoriesListView`: The placeholder view where the app stories will be displayed.

```xml
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">

  <LinearLayout
          android:layout_width="match_parent"
          android:layout_height="match_parent"
          android:layout_margin="20dp"
          android:orientation="vertical">


    <com.dengage.sdk.ui.story.StoriesListView
            android:id="@+id/stories_list_view"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_marginTop="12dp" />

  </LinearLayout>

</layout>
```

In your fragment or activity, set up the app stories using the `Dengage.showStoriesList` method.


```kotlin
class AppStoryFragment : BaseDataBindingFragment<FragmentAppStoryBinding>() {

  override fun getLayoutRes(): Int {
    return R.layout.fragment_app_story
  }

  override fun init() {
    val customParams = hashMapOf<String, String>()
    Dengage.showStoriesList(screenName = "screen-name",
      storiesListView = binding.storiesListView,
      storyPropertyId = "property-id",
      activity = requireActivity(),
      customParams = customParams
    )
  }

}
```

Parameters:

* **`screenName`** _(optional)_ Specifies the screen where the app stories should be displayed.
* **`storiesListView`** The `StoriesListView` defined in the XML layout where the stories will appear.
* **`storyPropertyId`** The story property ID associated with the app stories campaign created in the Dengage panel.
* **`customParams`** _(optional)_ A `HashMap` of custom parameters used for filtering stories.

## Geofence

The **Dengage Android Geofence SDK** enhances the **Dengage Android SDK** by integrating geofence capabilities through the **Play Services Location** library.

### Geofence Installation

The **Dengage Android Geofence SDK** is available via **JitPack**. To install the SDK, include the following dependencies in your `build.gradle` file. Ensure the `sdk-geofence` version matches the core SDK version.

```groovy
dependencies {
  implementation 'com.github.dengage-tech.dengage-android-sdk:sdk:6.0.93'
  implementation 'com.github.dengage-tech.dengage-android-sdk:sdk-geofence:6.0.93'
}
```

### Geofence Initialization

After initializing the core SDK with `Dengage.init`, you can enable geofence by calling the `DengageGeofence.startGeofence` method in your application class.

```kotlin
DengageGeofence.startGeofence()
```

### Request Location Permission

To request location permissions at runtime, use the `DengageGeofence.requestLocationPermissions` method.

```kotlin
DengageGeofence.requestLocationPermissions(activity)
```

### Geofence Interceptor

If you want to be notified inside your application whenever the user enters a monitored geofence region, you can register a `GeofenceInterceptor`. The interceptor's `onGeofenceEnter` callback is invoked alongside the SDK's default handling, giving you the chance to react to the event — for example, to log analytics, trigger custom UI, or show a local notification with information about the entered region.

Assign the interceptor to `DengageGeofence.geofenceInterceptor` **before** calling `DengageGeofence.startGeofence()` so that no events are missed when the app is launched in the background by a geofence trigger.

```kotlin
DengageGeofence.geofenceInterceptor = object : GeofenceInterceptor {
    override fun onGeofenceEnter(
        latitude: Double,
        longitude: Double,
        radius: Double,
        clusterId: Int,
        clusterName: String?,
        geofenceItemId: Int,
        geofenceItemName: String?
    ) {
        // Handle the geofence enter event, e.g. log it or show a local notification.
        Log.d(
            "GeofenceInterceptor",
            "enter | lat=$latitude, lon=$longitude, radius=$radius, " +
                "clusterId=$clusterId, clusterName=$clusterName, " +
                "itemId=$geofenceItemId, itemName=$geofenceItemName"
        )
    }
}

DengageGeofence.startGeofence()
```

#### Callback Parameters

| Parameter           | Type      | Description                                                                       |
|---------------------|-----------|-----------------------------------------------------------------------------------|
| `latitude`          | `Double`  | Latitude of the geofence region's center.                                         |
| `longitude`         | `Double`  | Longitude of the geofence region's center.                                        |
| `radius`            | `Double`  | Radius of the geofence region, in meters.                                         |
| `clusterId`         | `Int`     | Identifier of the geofence cluster the region belongs to.                         |
| `clusterName`       | `String?` | Human-readable name of the cluster, if provided on the server side.               |
| `geofenceItemId`    | `Int`     | Identifier of the specific geofence item that was entered.                        |
| `geofenceItemName`  | `String?` | Human-readable name of the geofence item, if provided on the server side.        |

> **Note**: The interceptor only adds your custom behavior; it does **not** replace the SDK's built-in geofence event reporting. The SDK will continue to send geofence events to Dengage regardless of whether an interceptor is registered.

### Performance Considerations and Best Practices

Geofence usage can have significant impacts on your application's performance and battery consumption. Here are important considerations:

#### App Launch Behavior

When a geofence is triggered while the app is not running (killed state), Android launches the app in the background and calls the `Application.onCreate()` method. This has important implications:

- **Initialization Order**: All code in `Application.onCreate()` will execute when a geofence triggers, even if the user hasn't explicitly opened the app. Ensure your initialization code handles this scenario gracefully.
- **Heavy Operations**: Avoid placing heavy synchronous operations (large data loads, complex UI setup, network calls that block the main thread) at the beginning of `Application.onCreate()`. These will delay geofence event processing.
- **BroadcastReceiver**: Geofence events are received by `GeofenceLocationReceiver`. The SDK automatically initializes Dengage if it hasn't been initialized yet when a geofence event is received.

#### How DengageGeofence SDK Works

DengageGeofence SDK uses Google Play Services Location API with the following characteristics:

- **Region Monitoring**: The SDK uses `FusedLocationProviderClient` and `GeofencingClient` for efficient geofence detection, which is battery-optimized by Google Play Services.
- **Maximum 50 Regions**: Google Play Services limits apps to monitor up to 50 geofences simultaneously. The SDK automatically manages this limit by selecting the 50 nearest geofences to the user's current location using Haversine distance calculation.
- **Automatic Region Updates**: When the user moves, the SDK recalculates and updates the monitored regions to always track the nearest 50 geofences from the server.
- **Balanced Power Accuracy**: The SDK uses `PRIORITY_BALANCED_POWER_ACCURACY` for location updates, providing a good balance between accuracy and battery consumption.
- **Rate Limiting**: Geofence data is fetched from the server at most every 15 minutes, and event signals for the same geofence are rate-limited to once every 5 minutes.
- **Boot Persistence**: The SDK automatically re-registers geofences after device reboot via `BOOT_COMPLETED` broadcast receiver.
- **Accuracy Filtering**: Location updates with accuracy worse than 1000 meters are automatically filtered out.

#### Recommendations

1. **Lazy Initialization**: Defer non-essential service initialization in your Application class:

```kotlin
class App : Application() {
    override fun onCreate() {
        super.onCreate()

        // Register lifecycle callbacks
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

        // Always initialize Dengage SDK first
        Dengage.init(
            context = applicationContext,
            firebaseIntegrationKey = "your-firebase-integration-key"
        )

        // Initialize geofence
        DengageGeofence.startGeofence()

        // Defer heavy initialization - these will only run when
        // an Activity is created (user actually opens the app)
    }
}
```

2. **Minimize Background Work**: When the app is launched due to a geofence trigger, minimize the work performed:
   - Avoid UI-related operations in `Application.onCreate()`
   - Skip non-essential network requests
   - Don't load large datasets into memory

3. **Handle Permissions Properly**: Request location permissions at an appropriate time in your app flow:
   - `ACCESS_FINE_LOCATION` is required for geofence functionality
   - `ACCESS_BACKGROUND_LOCATION` (Android 10+) is required for geofence triggers when the app is not in foreground

4**Battery Optimization Whitelist**: On some devices, aggressive battery optimization may affect geofence delivery. Consider guiding users to exclude your app from battery optimization if geofence reliability is critical.

> **Note**: If your app doesn't require geofence functionality, avoid including the `sdk-geofence` module to prevent unnecessary location permission requests and battery consumption.


## Huawei Messaging Service


### HMS setup

1. Complete the [HMS Android Setup](https://developer.huawei.com/consumer/en/codelab/HMSPushKit/index.html#3) to configure your Android application for Huawei integration.
2. Download the `agconnect-services.json` configuration file and place it in your app's directory.
3. In your root-level (project-level) Gradle file `<project>/build.gradle`, add the following dependency:

    ```groovy
    buildscript {
      repositories {
          maven {url 'http://developer.huawei.com/repo/'}
      }
      dependencies {        
          classpath "com.huawei.agconnect:agcp:1.9.1.300"
      }
    }
    ```

4. In your module (app-level) Gradle file `<project>/app/build.gradle`, apply the Huawei Agconnect plugin as follows:

   ```groovy
   plugins {
       id("com.huawei.agconnect")
   }
   ```

5. The **Dengage Android HMS SDK** is available via **JitPack**. To install the SDK, include the following dependencies in your `build.gradle` file. Ensure the `sdk-hms` version matches the core SDK version.

```groovy
dependencies {
  implementation 'com.github.dengage-tech.dengage-android-sdk:sdk:6.0.93'
  implementation 'com.github.dengage-tech.dengage-android-sdk:sdk-hms:6.0.93'
}
```

6. To handle push messages, you need to include the **HmsMessagingService** in your `AndroidManifest.xml` file. Place the following block inside the `<application>` tag of your `AndroidManifest.xml` file to ensure proper integration:

   ```xml
   <!-- Add the Hms Messaging Service to handle push notifications from HMS -->
   <service
      android:name="com.dengage.hms.HmsMessagingService"
      android:exported="false" >
      <intent-filter>
          <action android:name="com.huawei.push.action.MESSAGING_EVENT" />
      </intent-filter>
   </service>
   ```

### Initialization for Huawei

`init` method should be called once during the application's lifecycle to initialize the Dengage SDK. It is recommended to place it inside the `onCreate` method of your `Application` class.

```kotlin
val dengageHmsManager = DengageHmsManager()

Dengage.init(
  context = applicationContext,
  huaweiIntegrationKey = "your-huawei-integration-key",
  dengageHmsManager = dengageHmsManager,
  deviceConfigurationPreference = DeviceConfigurationPreference.Huawei,
  disableOpenWebUrl = false
)
```

Use `setHuaweiIntegrationKey` if you manage a Huawei build and need to provide or rotate the Dengage Huawei app integration key after initialization.

```kotlin
Dengage.setHuaweiIntegrationKey(
    integrationKey = "your-huawei-integration-key"
)
```

Just as like in [Carousel Push](#carousel-push) section, you need to define a custom receiver in your `AndroidManifest.xml` file and to prepare custom layouts for carousel functionality.
