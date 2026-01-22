# Changelog

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
