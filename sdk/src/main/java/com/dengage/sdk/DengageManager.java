package com.dengage.sdk;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.location.Location;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.dengage.sdk.callback.DengageCallback;
import com.dengage.sdk.domain.configuration.model.AppTracking;
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource;
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage;
import com.dengage.sdk.domain.push.model.Message;
import com.dengage.sdk.domain.rfm.model.RFMGender;
import com.dengage.sdk.domain.rfm.model.RFMItem;
import com.dengage.sdk.domain.rfm.model.RFMScore;
import com.dengage.sdk.domain.subscription.model.Subscription;
import com.dengage.sdk.domain.tag.model.TagItem;
import com.dengage.sdk.manager.geofence.GeofencePermissionsHelper;
import com.dengage.sdk.util.ContextHolder;
import com.dengage.sdk.util.DengageLogger;
import com.google.firebase.FirebaseApp;

import org.json.JSONObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DengageManager {

    @SuppressLint("StaticFieldLeak")
    private static DengageManager _instance = null;
    private final Context _context;
    private static String firebaseIntegrationKey = null;
    private static boolean geofenceStatus = false;

    private DengageManager(Context context) {
        _context = context;
        ContextHolder.INSTANCE.setContext(context);
    }

    /**
     * Singleton Object
     * <p>
     * Use to create dEngage MobileManager.
     *
     * @return DengageManager
     * </p>
     */
    public static DengageManager getInstance(Context context) {
        if (context == null) {
            throw new IllegalArgumentException("Argument null: context");
        }
        if (_instance == null)
            _instance = new DengageManager(context);

        return _instance;
    }

    public DengageManager setDeviceId(String deviceId) {
        Dengage.INSTANCE.setDeviceId(deviceId);
        return _instance;
    }

    public DengageManager setCountry(String country) {
        Dengage.INSTANCE.setCountry(country);
        return _instance;
    }

    /**
     * FirebaseApp Initiator method
     * <p>
     * Use to init Firebase Messaging
     *
     * @return DengageManager
     * </p>
     */
    public DengageManager init() {
        try {
            Dengage.INSTANCE.init(_context, firebaseIntegrationKey,  null,geofenceStatus,"");
        } catch (Exception e) {
            DengageLogger.INSTANCE.error(e.getMessage());
        }
        return _instance;
    }

    /**
     * FirebaseApp Initiator method
     * <p>
     * Use to init Firebase Messaging with instance
     *
     * @return DengageManager
     * </p>
     */
    public DengageManager initWithFirebaseInstance(@NonNull FirebaseApp firebaseApp) {
        try {
            // create in app message manager and start new session
            Dengage.INSTANCE.init(_context, firebaseIntegrationKey,  firebaseApp,geofenceStatus,"");
        } catch (Exception e) {
            DengageLogger.INSTANCE.error(e.getMessage());
        }
        return _instance;
    }


    /**
     * Deprecated method, use setUserPermission method
     */
    @Deprecated
    public void setPermission(Boolean permission) {
        setUserPermission(permission);
    }

    /**
     * Set contact key of the user.
     * <p>
     * Use to set dEngage key to a user.
     * </p>
     *
     * @param contactKey user key
     */
    public void setContactKey(String contactKey) {
        Dengage.INSTANCE.setContactKey(contactKey);
    }

    public DengageManager setFirebaseIntegrationKey(String key) {
        firebaseIntegrationKey = key;
        return _instance;
    }



    /**
     * Subscribe User
     * <p>
     * Use to register a user to dEngage. Only required when you perform a manuel GCM registration.
     * </p>
     *
     * @param token GCM Token
     */
    public void subscribe(String token) {
        DengageLogger.INSTANCE.verbose("subscribe(token) method is called");
        try {
            if (TextUtils.isEmpty(token))
                throw new IllegalArgumentException("Argument empty: token");
            Dengage.INSTANCE.setToken(token);

        } catch (Exception e) {
            DengageLogger.INSTANCE.error("subscribe(token): " + e.getMessage());
        }
    }

    /**
     * @return Subscription Object from the saved json.
     */
    public Subscription getSubscription() {
        return Dengage.INSTANCE.getSubscription();
    }

    public void buildSubscription() {
        try {
            Dengage.INSTANCE.getSubscriptionManager().buildSubscription(firebaseIntegrationKey);
        } catch (Exception ex) {
            DengageLogger.INSTANCE.error("buildSubscription: " + ex.getMessage());

        }
    }

    public void saveSubscription() {
        DengageLogger.INSTANCE.verbose("saveSubscription method is called");
        try {
            Dengage.INSTANCE.getSubscriptionManager().saveSubscription(Objects.requireNonNull(Dengage.INSTANCE.getSubscription()));
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("saveSubscription: " + e.getMessage());
        }
    }


    /**
     * Sends open event
     * <p>
     * Use to open report when a GCM message is received. Only required when you perform a manuel
     * GCM registration.
     * </p>
     *
     * @param message The dEngage message object.
     */
    public void sendOpenEvent(String buttonId, String itemId, Message message) {
        try {
            DengageLogger.INSTANCE.verbose("sendOpenEvent method is called");
            DengageLogger.INSTANCE.verbose(buttonId);
            DengageLogger.INSTANCE.verbose(itemId);
            Dengage.INSTANCE.sendOpenEvent(buttonId, itemId, message);


        } catch (Exception e) {
            DengageLogger.INSTANCE.error("sendOpenEvent: " + e.getMessage());
        }
    }

    /**
     * Console Log
     * <p>
     * Use to show logs on console.
     * </p>setLogStatus
     *
     * @param status True/False
     */
    public DengageManager setLogStatus(Boolean status) {
        Dengage.INSTANCE.setLogStatus(status);
        return _instance;
    }

    /**
     * Sends a custom event
     * <p>
     * Use to hit a custom event report.
     * </p>
     *
     * @param tableName The event table name of the schema.
     * @param key       Value of the event key.
     * @param data      Additional key-value data which is correspond table column name-value.
     */
    public void sendCustomEvent(String tableName, String key, HashMap<String, Object> data) {
        DengageLogger.INSTANCE.verbose("sendCustomEvent method is called");
        try {

            Dengage.INSTANCE.sendCustomEvent(tableName, key, data,_context);
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("sendCustomEvent: " + e.getMessage());
        }
    }

    /**
     * Sends a device event
     * <p>
     * Use to hit a device event report.
     * </p>
     *
     * @param tableName The event table name of the schema.
     * @param data      Additional key-value data which is correspond table column name-value.
     */
    public void sendDeviceEvent(String tableName, HashMap<String, Object> data) {
        DengageLogger.INSTANCE.verbose("sendDeviceEvent method is called");
        try {

            Dengage.INSTANCE.sendDeviceEvent(tableName, data,_context);
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("sendDeviceEvent: " + e.getMessage());
        }
    }

    public void onNewToken(String token) {
        try {
            DengageLogger.INSTANCE.debug("On new token : " + token);
            if (!TextUtils.isEmpty(token)) {
                DengageLogger.INSTANCE.debug("Send subscribe");
                Dengage.INSTANCE.onNewToken(token);
            }
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("onNewToken: " + e.getMessage());
        }
    }

    public void onMessageReceived(Map<String, String> data) {
        try {
            DengageLogger.INSTANCE.verbose("onMessageReceived method is called.");
            DengageLogger.INSTANCE.verbose("Raw Message: " + new JSONObject(data).toString());
            Dengage.INSTANCE.onMessageReceived(data);
        } catch (Exception e) {
        }
    }

    private void sendBroadcast(Context context, String json, Map<String, String> data) {
        DengageLogger.INSTANCE.verbose("sendBroadcast method is called.");
        try {
            Dengage.INSTANCE.sendBroadcast(json, data);
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("sendBroadcast: " + e.getMessage());
        }
    }

    private void getSdkParameters() {
        Dengage.INSTANCE.getConfigurationManager().getSdkParameters();
    }

    /**
     * Set User Push Permission
     * <p>
     * Use to set permission of current subscription
     * </p>
     *
     * @param permission True/False
     */
    public void setUserPermission(Boolean permission) {
        DengageLogger.INSTANCE.verbose("setUserPermission method is called");
        try {
            // control the last permission flag equals to new permission flag then send subscription
            Dengage.INSTANCE.setUserPermission(permission);
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("setUserPermission: " + e.getMessage());
        }
    }

    /**
     * Get User Push Permission
     * <p>
     * Use to get permission of current subscription
     * </p>
     */
    public @Nullable
    Boolean getUserPermission() {
        return Dengage.INSTANCE.getUserPermission();
    }

    /**
     * Set Token method
     * <p>
     * Use to set token of current subscription
     * </p>
     */
    public void setToken(@NonNull String token) {
        DengageLogger.INSTANCE.verbose("setToken method is called");
        try {
            Dengage.INSTANCE.setToken(token);


        } catch (Exception e) {
            DengageLogger.INSTANCE.error("setToken: " + e.getMessage());
        }
    }

    /**
     * Get Token method
     * <p>
     * Use to get token of current subscription
     * </p>
     */
    public @Nullable
    String getToken() {
        return Dengage.INSTANCE.getToken();
    }


    /**
     * Get saved inbox messages
     */
    public void getInboxMessages(@NonNull Integer limit, @NonNull final Integer offset,
                                 @NonNull final DengageCallback<List<InboxMessage>> dengageCallback) {
        Dengage.INSTANCE.getInboxMessages(limit, offset, dengageCallback);
    }

    /**
     * Delete inbox message
     *
     * @param id id of inbox message that will be deleted.
     */
    public void deleteInboxMessage(final String id) {
        // control inbox message enabled
        Dengage.INSTANCE.deleteInboxMessage(id);
    }

    /**
     * Mark inbox message as read
     *
     * @param id id of inbox message that will be marked as read.
     */
    public void setInboxMessageAsClicked(final String id) {
        // control inbox message enabled
        Dengage.INSTANCE.setInboxMessageAsClicked(id);
    }

    public void startAppTracking(List<AppTracking> appTrackings) {
        // tracking time will be 0 for first tracking
        Dengage.INSTANCE.startAppTracking(appTrackings);
    }

    public void getInAppMessages() {
        Dengage.INSTANCE.getInAppMessages();
    }

    /**
     * Show in app message if any available
     *
     * @param activity for showing dialog fragment as in app message
     */
    public void setNavigation(@NonNull Activity activity) {
        setNavigation(activity, null);
    }

    /**
     * Show in app message if any available
     *
     * @param activity   for showing dialog fragment as in app message
     * @param screenName for showing screen specific in app message
     */
    public void setNavigation(@NonNull Activity activity, @Nullable String screenName) {
        Dengage.INSTANCE.setNavigation(activity, screenName,-1);
    }

    /**
     * Send tags
     *
     * @param tags will be send to api
     */
    public void setTags(@NonNull List<TagItem> tags) {
        Dengage.INSTANCE.setTags(tags,_context);
    }

    /**
     * Set Notification Channel Name
     *
     * @param name will be saved in prefs as channel name
     */
    public void setNotificationChannelName(String name) {
        Dengage.INSTANCE.setNotificationChannelName(name);
    }

    /**
     * Use for saving rfm scores to local storage if you will use rfm item sorting
     */
    public void saveRFMScores(@Nullable List<RFMScore> scores) {
        Dengage.INSTANCE.saveRFMScores(scores);
    }

    /**
     * Use for updating score of category
     */
    public void categoryView(@NonNull String categoryId) {
        Dengage.INSTANCE.categoryView(categoryId,_context);
    }

    /**
     * Use for sorting rfm items with respect to rfm scores saved to local storage
     */
    public <T> List<T> sortRFMItems(RFMGender rfmGender, List<RFMItem> rfmItems) {
        return Dengage.INSTANCE.sortRFMItems(rfmGender, rfmItems);
    }

    /**
     * Send tags for ecomm2
     *
     * @param tags will be send to api
     */
    private void setTags(@NonNull List<TagItem> tags, String keyType) {
  /*      String key = "";
        SdkParameters sdkParameters = prefs.getSdkParameters();
        if (sdkParameters == null || sdkParameters.getAccountName() == null) {
            return;
        }
        if (keyType.equalsIgnoreCase("contact")) {
            key = _subscription.getContactKey();
            if (key.isEmpty()) return;
        } else if (keyType.equalsIgnoreCase("device")) {
            key = _subscription.getDeviceId();
        } else if (keyType.equalsIgnoreCase("ContactOrDevice")) {
            key = _subscription.getContactKey();
            if (key.isEmpty()) {
                key = _subscription.getDeviceId();
            }
        }
        // convert tags request to json string
        TagsRequest tagsRequest = new TagsRequest(
                sdkParameters.getAccountName(),
                key,
                tags
        );
        String postData = GsonHolder.INSTANCE.getGson().toJson(tagsRequest, TagsRequest.class);
        // call http request
        NetworkRequest networkRequest = new NetworkRequest(
                NetworkUrlUtils.INSTANCE.setTagsRequestUrl(_context),
                Utils.getUserAgent(_context),
                postData,
                null);
        networkRequest.executeTask();*/
    }

    public DengageManager setGeofenceStatus(boolean isEnabled) {
        DengageLogger.INSTANCE.verbose("Geofence method is called");
        try {
            geofenceStatus = isEnabled;
        } catch (Exception e) {
            DengageLogger.INSTANCE.error("Geofence: " + e.getMessage());
        }
        return _instance;
    }

    /**
     * Request location permission
     * <p>
     * Use to request location permission for geofence tracking.
     *
     * @param activity for showing ui of location permission request
     */
    public void requestLocationPermissions(Activity activity) {
        GeofencePermissionsHelper.INSTANCE.requestLocationPermissions(activity);
    }

    /**
     * Stop location tracking
     * <p>
     * Use to stop location geofence tracking.
     */
    public void stopGeofence() {
        Dengage.INSTANCE.stopGeofence();
    }

    public void startGeofence() {
        Dengage.INSTANCE.startGeofence();
    }

    public void handleLocation(Context context, Location location, GeofenceLocationSource source, String geofenceRequestId) {
        Dengage.INSTANCE.handleLocation(context, location, source, geofenceRequestId);
    }

    public void handleBootCompleted(Context context) {

        Dengage.INSTANCE.handleBootCompleted(context);
    }

}
