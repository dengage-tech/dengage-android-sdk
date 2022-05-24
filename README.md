# Dengage Android Sdk

## Useful Links

- [SDK Documentation](https://dev.dengage.com/mobile-sdk/android/)
- [Admin Panel](https://appdev.dengage.com/)

## Sdk Implementation

- Add dengage sdk dependency to your project level gradle file.

```groovy
dependencies {
    implementation 'com.github.dengage-tech:dengage-android-sdk:x.y.z'
}
```

## Sdk Necessary Rules

### Application Manifest Parameters

You should add some parameters with meta-data to your application manifest.

```
<!-- Event api url of Dengage -->
<meta-data
    android:name="den_event_api_url"
    android:value="https://dev-event.dengage.com" />

<!-- Push api url of Dengage -->
<meta-data
    android:name="den_push_api_url"
    android:value="https://pushdev.dengage.com" />
```

### Application Manifest Services

You should add firebase and huawei messaging services to your application manifest. If you are using
your own messaging service classes, you don't need to add Dengage service classes to your manifest.

```
<!-- Fcm Messaging Service for handling push messages comes from firebase -->
<service
    android:name="com.dengage.sdk.push.FcmMessagingService"
    android:exported="false">
    <intent-filter>
        <action android:name="com.google.firebase.MESSAGING_EVENT" />
    </intent-filter>
</service>

<!-- Hms Messaging Service for handling push messages comes from huawei messaging service -->
<service
    android:name="com.dengage.sdk.push.HmsMessagingService"
    android:exported="false">
    <intent-filter>
        <action android:name="com.huawei.push.action.MESSAGING_EVENT" />
    </intent-filter>
</service>
```

### Application Manifest Push Receiver

You should add your PushNotificationReceiver class which extends NotificationReceiver class of
Dengage sdk.

```
<!-- For handling push notifications comes to messaging service classes -->
<receiver
    android:name="{your_package}.PushNotificationReceiver"
    android:exported="false">
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

### Registering LifeCycle Callbacks

You should register activity lifecycle callbacks to DengageLifecycleTracker. This is required for
fetching some contents from api like in app message list. You can do this operation on your
application class.

```
registerActivityLifecycleCallbacks(DengageLifecycleTracker())
```

## Sdk Methods

### init

Should be called once in application lifecycle for initiation of Dengage sdk. You can place it into
Application class onCreate method.

```
Dengage.init(
    context: Context, /* Application context */
    firebaseIntegrationKey: String?, /* Your Dengage firebase app integration key */
    huaweiIntegrationKey: String?, /* Your Dengage huawei app integration key */
    firebaseApp: FirebaseApp? /* Your firebase app if you want to control push messages with custom firebase app integration (optional) */
)
```

### setLogStatus

You can enable or disable logs comes from Dengage sdk to your Logcat.

```
Dengage.setLogStatus(
    enable: Boolean /* To enable logs send true, to disable logs send false */
)
```

### setFirebaseIntegrationKey

You can also set your Dengage firebase app integration key after sdk initiation.

```
Dengage.setFirebaseIntegrationKey(
    integrationKey: String /* Your Dengage firebase app integration key */
)
```

### setHuaweiIntegrationKey

You can also set your Dengage huawei app integration key after sdk initiation.

```
Dengage.setHuaweiIntegrationKey(
    integrationKey: String /* Your Dengage huawei app integration key */
)
```

### getSubscription

You can get Dengage sdk subscription model from sdk cache if exists (If you init correctly, it will
exist).

```
Dengage.getSubscription(): Subscription?
```

### setDeviceId

You can set device id of current user subscription.

```
Dengage.setDeviceId(
    deviceId: String /* Unique identifier of device */
)
```

### setCountry

You can set country of current user subscription.

```
Dengage.setCountry(
    country: String /* Free country text */
)
```

### setContactKey

You can set contact key of current user subscription.

```
Dengage.setContactKey(
    contactKey: String? /* User key */
)
```

### setUserPermission

You can set user permission of current user subscription.

```
Dengage.setUserPermission(
    permission: Boolean /* User permission flag */
)
```

### getUserPermission

You can get user permission of current user subscription.

```
Dengage.getUserPermission(): Boolean?
```

### setToken

You can set firebase or huawei messaging token of current user subscription.

```
Dengage.setToken(
    token: String? /* Firebase or huawei messaging token */
)
```

### getToken

You can get token of current user subscription.

```
Dengage.getToken(): String?
```

### onNewToken

You can set firebase or huawei messaging token of current user subscription.

```
Dengage.onNewToken(
    token: String? /* Firebase or huawei messaging token */
)
```

### setNotificationChannelName

You can set notification channel name for setting to notifications above android 26.

```
Dengage.setNotificationChannelName(
    name: String /* Notification channel name */
)
```

### startAppTracking

You can start app tracking with package name of the apps that you want to track.

```
Dengage.startAppTracking(
    appTrackings: List<AppTracking>? /* App info list that you want to track */
)
```

### getInboxMessages

You can get inbox messages with pagination. If you set push message addToInbox parameter as true,
push messages sent from Dengage are also saved to your inbox.

```
Dengage.getInboxMessages(
    limit: Int, /* Size limit of the inbox list */
    offset: Int, /* Offset of the inbox list, you should send 0 to get first page */
    dengageCallback: DengageCallback<MutableList<InboxMessage>> /* Callback for getting inbox message list returned from Dengage api */
)
```

### deleteInboxMessage

You can delete any inbox message from the user's inbox.

```
Dengage.deleteInboxMessage(
    messageId: String /* Id of the inbox message that you want to delete from users inbox */
)
```

### setInboxMessageAsClicked

You can set any inbox message as clicked with the user's interaction.

```
Dengage.setInboxMessageAsClicked(
    messageId: String /* Id of the inbox message that user clicked */
)
```

### setNavigation

You can call this method for showing in app message popup if exists.

```
Dengage.setNavigation(
    activity: Activity /* For showing ui of in app message */
)
```

### setNavigation

You can call this method for showing in app message popup if exists with the given screen name.

```
Dengage.setNavigation(
    activity: Activity, /* For showing ui of in app message */
    screenName: String? /* For filtering in app messages with respect to current screen in your app */
)
```

### setTags

You can send tag items to Dengage api.

```
Dengage.setTags(
    tags: List<TagItem> /* TagItem list that you want to send Dengage api */
)
```

### onMessageReceived

You should send push message data, if you are using your own firebase or huawei messaging service
receivers.

```
Dengage.onMessageReceived(
    data: Map<String, String?>? /* Data of the push message comes to your firebase or huawei messaging service */
)
```

### showTestPage

You can use our embedded test pages if you want to show or manipulate any data on Dengage sdk. It is
useful for your development or qa team. Test pages contain;

- Push Message ui and events
- In App Message ui and events
- Getting your device's info on Dengage sdk (Contains copy paste)
- Getting your device's cache on Dengage sdk (Contains copy paste)
- Manipulating fetch times of device parameters
- Showing last 200 logs of Dengage sdk operations (Contains copy paste)

```
Dengage.showTestPage(
    activity: Activity /* For showing ui of test pages */
)
```

### saveRFMScores

You can save rfm scores to local storage if you will use rfm item sorting.

```
Dengage.saveRFMScores(
    scores: RFMScore(
        categoryId: String, /* Category identifier of the RFMScore object */
        score: Double /* The score value of the RFMScore object, specified as a value from 0.0 to 1.0. 
                         Score values below 0.0 are interpreted as 0.0, and values above 1.0 are interpreted as 1.0 */
    )
)
```

### categoryView

You can update rfm score of viewed category.

```
Dengage.categoryView(
    categoryId: String /* Category identifier of viewed category */
)
```

### sortRFMItems

You can sort rfm items with respect to rfm scores saved to local storage.
Returns list of the sorted rfm items

```
Dengage.sortRFMItems(
    rfmGender: RFMGender,
    rfmItems: MutableList<RFMItem>
)

RFMItem(
    id: String,
    categoryId: String,
    personalized: Boolean,
    gender: RFMGender{
        MALE,
        FEMALE,
        NEUTRAL
    },
    sequence: Int
)
```

You can use RFMItem directly. Also you can create your own rfm item object that extends RFMItem.
Then sort your own items like below:

```
class YourOwnRFMItem(
    id: String,
    categoryId: String,
    personalized: Boolean,
    gender: RFMGender,
    sequence: Int,
    var parameter1: String,
    var parameter2: String,
) : RFMItem(
    id = id,
    categoryId = categoryId,
    personalized = personalized,
    gender = gender,
    sequence = sequence,
)

val sortedYourOwnRFMItems: MutableList<YourOwnRFMItem> = Dengage.sortRFMItems(
    rfmItems = listOfYourOwnRFMItems
)
```

## Event Methods

You can send events to Dengage sdk. Available event methods listed below. But you can send your own
custom events also.

```
Dengage.pageView(data: HashMap<String, Any>)

Dengage.sendCartEvents(
    data: HashMap<String, Any>,
    eventType: String
)

Dengage.addToCart(data: HashMap<String, Any>)

Dengage.removeFromCart(data: HashMap<String, Any>)

Dengage.viewCart(data: HashMap<String, Any>)

Dengage.beginCheckout(data: HashMap<String, Any>)

Dengage.cancelOrder(data: HashMap<String, Any>)

Dengage.order(data: HashMap<String, Any>)

Dengage.search(data: HashMap<String, Any>)

Dengage.sendWishListEvents(
    data: HashMap<String, Any>,
    eventType: String
)

Dengage.addToWishList(data: HashMap<String, Any>)

Dengage.removeFromWishList(data: HashMap<String, Any>)

Dengage.sendCustomEvent(
    tableName: String,
    key: String,
    data: HashMap<String, Any>
)

Dengage.sendDeviceEvent(
    tableName: String,
    data: HashMap<String, Any>
)

Dengage.sendOpenEvent(
    buttonId: String,
    itemId: String,
    message: Message?
)
```

## Sample App Content

1- Adding Manifest Rules of Sdk (See [AndroidManifest](app/src/main/AndroidManifest.xml))

2- Initiating Dengage Sdk (See
class [App](app/src/main/java/com/dengage/android/kotlin/sample/App.kt))

3- Push Notification Receiver (See
class [PushNotificationReceiver](app/src/main/java/com/dengage/android/kotlin/sample/push/PushNotificationReceiver.kt))

4- Sending Page View Events (See
class [BaseDataBindingFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/base/BaseDataBindingFragment.kt))

5- Showing Device Subscription Parameters (See
class [DeviceInfoFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/DeviceInfoFragment.kt))

6- Changing Contact Key (See
class [ContactKeyFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/ContactKeyFragment.kt))

7- Listing Inbox Messages & Actions of Inbox Messages (See
class [InboxMessagesFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/InboxMessagesFragment.kt))

8- Sending Custom Events (See
class [CustomEventFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/CustomEventFragment.kt))

9- In App Message (See
class [InAppMessageFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/InAppMessageFragment.kt))

10- Send Tags (See
class [TagsFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/TagsFragment.kt))

11- Show Dengage Test Page (See
class [HomeFragment](app/src/main/java/com/dengage/android/kotlin/sample/ui/fragment/HomeFragment.kt))

## Migration From Old Sdk

You can follow the instructions on this documentation for migration from old sdk of Dengage.
Visit [here](migration-from-old-sdk.md)
