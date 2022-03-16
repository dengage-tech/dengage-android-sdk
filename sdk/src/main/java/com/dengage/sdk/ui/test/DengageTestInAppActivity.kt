package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.*
import java.util.*

class DengageTestInAppActivity : Activity(), View.OnClickListener {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_dengage_test_in_app)

        findViewById<Button>(R.id.btnFullScreen1).setOnClickListener(this)
        findViewById<Button>(R.id.btnFullScreen2).setOnClickListener(this)
        findViewById<Button>(R.id.btnBanner1).setOnClickListener(this)
        findViewById<Button>(R.id.btnBanner2).setOnClickListener(this)
        findViewById<Button>(R.id.btnModal1).setOnClickListener(this)
        findViewById<Button>(R.id.btnModal2).setOnClickListener(this)
        findViewById<Button>(R.id.btnModal3).setOnClickListener(this)
        findViewById<Button>(R.id.btnModal4).setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        val inAppMessage = when (v?.id) {
            R.id.btnFullScreen1 -> {
                generateFullScreen1()
            }
            R.id.btnFullScreen2 -> {
                generateFullScreen2()
            }
            R.id.btnBanner1 -> {
                generateBanner1()
            }
            R.id.btnBanner2 -> {
                generateBanner2()
            }
            R.id.btnModal1 -> {
                generateModal1()
            }
            R.id.btnModal2 -> {
                generateModal2()
            }
            R.id.btnModal3 -> {
                generateModal3()
            }
            R.id.btnModal4 -> {
                generateModal4()
            }
            else -> null
        }

        inAppMessage?.let {
            var inAppMessages = Prefs.inAppMessages
            if (inAppMessages == null) {
                inAppMessages = mutableListOf()
            }
            inAppMessages.add(0, it)
            Prefs.inAppMessages = inAppMessages

            Dengage.setNavigation(this)
        }
    }

    private fun generateFullScreen1(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.FULL.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n  height: 100%;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n  height: 100%;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n  height: 100%;\n  display: flex;\n  flex-direction: column;\n}\n.content {\n    flex: 1;\n    display: flex;\n    flex-direction: column;\n}\n.img-c {\n    width: 100%;\n    flex: 1;\n    background-image: url(\"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\");\n    background-color: #cccccc; /* Used if the image is unavailable */\n    background-position: center; \n    background-repeat: no-repeat;\n    background-size: cover;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    padding: 14px;\n    flex: 1;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n    margin-top: -52px;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: black;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"img-c\">\n            <!--<img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">-->\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 10000,
            radius = 0,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 0,
            marginRight = 0,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateFullScreen2(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.FULL.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n  height: 100%;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n  height: 100%;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n  height: 100%;\n  display: flex;\n  flex-direction: column;\n}\n.content {\n    flex: 1;\n    display: flex;\n    flex-direction: column;\n}\n.img-c {\n    width: 100%;\n    flex: 1;\n    background-image: url(\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\"); /* The image used */\n    background-color: #cccccc; /* Used if the image is unavailable */\n    background-position: center; \n    background-repeat: no-repeat;\n    background-size: cover;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    padding: 14px;\n    flex: 1;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: white;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"img-c\">\n            <!--<img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">-->\n        </div>\n        <div class=\"content\">\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n            <div class=\"buttons\">\n                <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                    Primary\n                </button>\n                <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                    Secondary\n                </button>\n            </div>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 10000,
            radius = 0,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 0,
            marginRight = 0,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateBanner1(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.TOP.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n  padding: 12px;\n}\n.content {\n    display: flex;\n}\n.img-c {\n    width: 22%;\n    max-width: 100px;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding-left: 12px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    margin-top: 12px;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: #1C2C48;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://play-lh.googleusercontent.com/IeNJWoKYx1waOhfWF6TiuSiWBLfqLb18lmZYXSgsH1fvb8v1IYiZr5aYWe0Gxu-pVZX3\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 450,
            radius = 5,
            marginTop = 5,
            marginBottom = 5,
            marginLeft = 5,
            marginRight = 5,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateBanner2(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.BOTTOM.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n  padding: 12px;\n}\n.content {\n    display: flex;\n}\n.img-c {\n    width: 22%;\n    max-width: 100px;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding-left: 12px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    margin-top: 12px;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: #1C2C48;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://play-lh.googleusercontent.com/IeNJWoKYx1waOhfWF6TiuSiWBLfqLb18lmZYXSgsH1fvb8v1IYiZr5aYWe0Gxu-pVZX3\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 450,
            radius = 5,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 0,
            marginRight = 0,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateModal1(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.MIDDLE.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n}\n.content {\n    \n}\n.img-c {\n    width: 100%;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding: 14px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: white;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 300,
            radius = 0,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 5,
            marginRight = 5,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateModal2(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.MIDDLE.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n}\n.content {\n    \n}\n.img-c {\n    width: 100%;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding: 14px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: white;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 300,
            radius = 5,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 5,
            marginRight = 5,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateModal3(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.MIDDLE.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n}\n.content {\n    \n}\n.img-c {\n    width: 100%;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding: 14px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: white;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 300,
            radius = 0,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 5,
            marginRight = 5,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }

    private fun generateModal4(): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.MIDDLE.position,
            shouldAnimate = true,
            html = "<!DOCTYPE html>\n<html>\n<head>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<meta charset=\"UTF-8\">\n<style>\nhtml {\n  box-sizing: border-box;\n  margin: 0;\n  padding: 0;\n}\n*, *:before, *:after {\n  box-sizing: inherit;\n}\nbody {\n  margin: 0;\n  padding: 0;\n  font-family: Arial, Helvetica, sans-serif;\n}\nimg {\n  max-width: 100%;\n  height: auto;\n  display: block;\n}\n\n.container {\n  width: 100%;\n}\n.content {\n    \n}\n.img-c {\n    width: 100%;\n}\n.img-c img {\n    width: 100%;\n}\n.body-c {\n    flex: 1;\n    padding: 14px;\n}\n.title {\n    color: #1C2C48;\n    font-weight: bold;\n    font-size: 16px;\n    line-height: 24px;\n}\n.message {\n    color: #1C2C48;\n    font-weight: normal;\n    font-size: 14px;\n    \n}\n\n.buttons {\n    display: flex;\n    padding: 12px;\n    padding-top: 0;\n}\n.primaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #125CFA;\n    text-align: center;\n    cursor: pointer;\n    color: white;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    text-decoration:none;\n}\n\n.secondaryBtn {\n    display: inline-block;\n    flex: 1;\n    min-width: 60px;\n    font-style: normal;\n    height: 40px;\n    line-height: 24px;\n    background-color: #EEF2F5;\n    text-align: center;\n    cursor: pointer;\n    color: #125CFA;\n    font-size: 14px;\n    font-weight: normal;\n    padding: 8px 16px;\n    border-radius: 8px;\n    border-width: 0px;\n    outline: none;\n    margin-left: 12px;\n    text-decoration:none;\n}\n.closeBtn {\n    position: absolute;\n    right: 12px;\n    top: 4px;\n    font-size: 22px;\n    display: inline-block;\n    text-decoration:none;\n    outline: none;\n    color: white;\n}\n</style>\n</head>\n<body>\n    <div class=\"container\">\n        <div class=\"content\">\n            <div class=\"img-c\">\n                <img src=\"https://ccn.waag.org/drupal/sites/default/files/2018-03/campaign-blog-graphic-01-1080x675.jpg\" alt=\"\">\n            </div>\n            <div class=\"body-c\">\n                <div class=\"title\">\n                    Title goes here\n                </div>\n                <div class=\"message\">\n                    Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text\n                </div>\n            </div>\n        </div>\n        <div class=\"buttons\">\n            <button type=\"button\" class=\"primaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('primary'); Dn.close();\">\n                Primary\n            </button>\n            <button type=\"button\" class=\"secondaryBtn\" onclick=\"Dn.androidUrl('https://example.com'); Dn.iosUrl('https://example.com'); Dn.sendClick('secondary'); Dn.close();\">\n                Secondary\n            </button>\n        </div>\n        <a href=\"#\" class=\"closeBtn\" onclick=\"Dn.dismiss()\">\n            &times;\n        </a>\n    </div>\n</body>\n</html>\n",
            maxWidth = 300,
            radius = 10,
            marginTop = 0,
            marginBottom = 0,
            marginLeft = 5,
            marginRight = 5,
            dismissOnTouchOutside = true
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )
        val displayCondition = DisplayCondition(
            screenNameFilters = null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 0,
            showEveryXMinutes = 0
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = "2030-10-11T12:00:00.000Z",
            priority = Priority.HIGH.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = UUID.randomUUID().toString(),
            data = inAppMessageData
        )
    }
}