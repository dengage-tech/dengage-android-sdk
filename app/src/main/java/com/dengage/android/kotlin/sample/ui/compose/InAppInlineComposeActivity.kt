package com.dengage.android.kotlin.sample.ui.compose

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import com.dengage.sdk.Dengage

/**
 * Sample Compose Activity for testing In-App Inline messages
 * Similar to InAppInLineFragment but using Jetpack Compose
 */
class InAppInlineComposeActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme {
                InAppInlineComposeScreen()
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun InAppInlineComposeScreen() {
    var propertyId by remember { mutableStateOf("1") }
    var screenName by remember { mutableStateOf("inline") }
    var customKey1 by remember { mutableStateOf("") }
    var customValue1 by remember { mutableStateOf("") }
    var customKey2 by remember { mutableStateOf("") }
    var customValue2 by remember { mutableStateOf("") }
    var inAppMessage by remember { mutableStateOf<com.dengage.sdk.domain.inappmessage.model.InAppMessage?>(null) }
    var isLoading by remember { mutableStateOf(false) }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("In-App Inline (Compose)") }
            )
        }
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(paddingValues)
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp)
        ) {
            // Property ID Input
            OutlinedTextField(
                value = propertyId,
                onValueChange = { propertyId = it },
                label = { Text("Property ID") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true
            )

            // Screen Name Input
            OutlinedTextField(
                value = screenName,
                onValueChange = { screenName = it },
                label = { Text("Screen Name") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true
            )

            // Custom Parameters Row 1
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                OutlinedTextField(
                    value = customKey1,
                    onValueChange = { customKey1 = it },
                    label = { Text("Custom Key 1") },
                    modifier = Modifier.weight(1f),
                    singleLine = true
                )
                OutlinedTextField(
                    value = customValue1,
                    onValueChange = { customValue1 = it },
                    label = { Text("Custom Value 1") },
                    modifier = Modifier.weight(1f),
                    singleLine = true
                )
            }

            // Custom Parameters Row 2
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                OutlinedTextField(
                    value = customKey2,
                    onValueChange = { customKey2 = it },
                    label = { Text("Custom Key 2") },
                    modifier = Modifier.weight(1f),
                    singleLine = true
                )
                OutlinedTextField(
                    value = customValue2,
                    onValueChange = { customValue2 = it },
                    label = { Text("Custom Value 2") },
                    modifier = Modifier.weight(1f),
                    singleLine = true
                )
            }

            // Get activity context
            val activity = androidx.compose.ui.platform.LocalContext.current as? ComponentActivity
            
            // Remember InAppInlineElement
            val inAppInlineElement = remember {
                activity?.let {
                    com.dengage.sdk.ui.inappmessage.InAppInlineElement(it)
                }
            }

            // Show Inline In-App Button
            Button(
                onClick = {
                    if (activity != null && inAppInlineElement != null) {
                        isLoading = true
                        val customParams = hashMapOf<String, String>()
                        if (customKey1.isNotEmpty() && customValue1.isNotEmpty()) {
                            customParams[customKey1] = customValue1
                        }
                        if (customKey2.isNotEmpty() && customValue2.isNotEmpty()) {
                            customParams[customKey2] = customValue2
                        }

                        Dengage.showInlineInApp(
                            screenName = screenName.ifEmpty { null },
                            inAppInlineElement = inAppInlineElement,
                            propertyId = propertyId,
                            activity = activity,
                            customParams = customParams.ifEmpty { null }
                        )
                        isLoading = false
                    }
                },
                modifier = Modifier.fillMaxWidth(),
                enabled = !isLoading && activity != null && inAppInlineElement != null
            ) {
                if (isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(20.dp),
                        color = MaterialTheme.colorScheme.onPrimary
                    )
                    Spacer(modifier = Modifier.width(8.dp))
                }
                Text("Show In-App Inline")
            }

            // In-App Inline Element Display Area
            Card(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(200.dp),
                elevation = CardDefaults.cardElevation(defaultElevation = 4.dp)
            ) {
                if (inAppInlineElement != null) {
                    AndroidView(
                        factory = { inAppInlineElement },
                        modifier = Modifier.fillMaxSize()
                    )
                } else {
                    Box(
                        modifier = Modifier.fillMaxSize(),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = "In-App Inline Element will appear here",
                            style = MaterialTheme.typography.bodyMedium
                        )
                    }
                }
            }

            Spacer(modifier = Modifier.weight(1f))
        }
    }
}

