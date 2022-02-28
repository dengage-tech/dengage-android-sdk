# Migration From Old Dengage Sdk

## Class Imports

You can change class imports with respect to table below.

| Old Sdk | New Sdk |
| :---    |  :---   |
| com.dengage.sdk.DengageLifecycleTracker | com.dengage.sdk.util.DengageLifecycleTracker |
| com.dengage.sdk.NotificationReceiver | com.dengage.sdk.push.NotificationReceiver |
| com.dengage.sdk.FcmMessagingService | com.dengage.sdk.push.FcmMessagingService |
| com.dengage.sdk.HmsMessagingService | com.dengage.sdk.push.HmsMessagingService |
| com.dengage.sdk.Constants | com.dengage.sdk.util.Constants |
| com.dengage.sdk.models.Message | com.dengage.sdk.domain.push.model.Message |
| com.dengage.sdk.models.CarouselItem | com.dengage.sdk.domain.push.model.CarouselItem |
| com.dengage.sdk.models.InboxMessage | com.dengage.sdk.domain.inboxmessage.model.InboxMessage |
| com.dengage.sdk.models.Subscription | com.dengage.sdk.domain.subscription.model.Subscription |
| com.dengage.sdk.models.DengageError | com.dengage.sdk.callback.DengageError |
| com.dengage.sdk.models.TagItem | com.dengage.sdk.domain.tag.model.TagItem |

## Renamed DengageManager Class

Sdk's main class named ```DengageManager``` is renamed as ```Dengage```. Also you don't need to get
instance of ```DengageManager``` class and keep it as parameter anymore. You can just call functions
of sdk like ```Dengage.functionName```.

## Removal of DengageEvent Class

```DengageEvent``` class removed. ```DengageEvent``` functions moved to ```Dengage``` class.

Example:

```
// In old sdk 
DengageEvent.getInstance(context).pageView(data) 

// In new sdk
Dengage.pageView(data)
```

## Initiation of Dengage

Changed initiation of the sdk. You should just call init function with parameters.

Example:

```
// In old sdk 
DengageManager
    .getInstance(applicationContext)
    .setFirebaseIntegrationKey(FIREBASE_APP_INTEGRATION_KEY)
    .init()

// In new sdk
Dengage.init(
    context = applicationContext,
    firebaseIntegrationKey = FIREBASE_APP_INTEGRATION_KEY,
    huaweiIntegrationKey = HUAWEI_APP_INTEGRATION_KEY, /* Optional */
    firebaseApp: FirebaseApp? /* Optional */
)
```

## Removed Methods and Alternatives

We removed, renamed or moved some methods in new sdk. You should make changes for migration with
respect to table below.

| Old Sdk | New Sdk | Explanation |
| :---    |  :---   |  :---   |
| DengageManager.initWithFirebaseInstance | Dengage.init | New init method contains an optional parameter named firebaseApp |
| DengageManager.isGooglePlayServicesAvailable | ConfigurationUtils.isGooglePlayServicesAvailable | Method moved from manager class to utils of configuration |
| DengageManager.isHuaweiMobileServicesAvailable | ConfigurationUtils.isHuaweiMobileServicesAvailable | Method moved from manager class to utils of configuration |
| DengageManager.setPermission | Dengage.setUserPermission | Method is removed, you should use new one |
| DengageManager.subscribe | Dengage.setToken | Method is removed, you should use new one |
| DengageManager.buildSubscription | - | You don't need to call this function anymore |
| DengageManager.saveSubscription | - | You don't need to call this function anymore |
| DengageManager.syncSubscription | - | You don't need to call this function anymore |

## NotificationReceiver Updated

- Simplified carousel rendering. We have only one function named ```onCarouselRender``` for setting
  push messages with carousel content. Removed old method named ```onCarouselReRender```. Changed
  image downloading methods of sdk. Removed some calculations for carousel content. Our sample
  receiver [PushNotificationReceiver](app/src/main/java/com/dengage/android/kotlin/sample/push/PushNotificationReceiver.kt)

- You can override rich or text notification methods on your notification receiver class for using
  custom notifications (Optional)
  Example:

```
override fun onRichNotificationRender(
    context: Context,
    intent: Intent,
    message: Message,
    bitmap: Bitmap,
    notificationBuilder: NotificationCompat.Builder
) {
    // do not call super
    // do your custom implementation
}

override fun onTextNotificationRender(
    context: Context, 
    intent: Intent, 
    message: Message, 
    notificationBuilder: NotificationCompat.Builder
) {
    // do not call super
    // do your custom implementation
}
```
