package com.dengage.sdk.ui.inappmessage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.net.http.SslError;
import android.os.Build;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.webkit.SslErrorHandler;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ProgressBar;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.dengage.sdk.R;

import java.util.ArrayList;
import java.util.List;


public class InAppBrowserActivity extends AppCompatActivity implements
        CustomSwipeRefreshLayout.CanChildScrollUpCallback, CustomSwipeRefreshLayout.OnRefreshListener {

    private static String INTENT_EXTRA_URL = "url";

    private String mTitle = "";
    private String mUrl;

    private List<String> mTitles;
    private List<String> mUrls;

    private CustomSwipeRefreshLayout mSwipeRefresh;
    private Toolbar mToolbar;
    private ProgressBar mProgress;
    private WebView mWebview;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_in_app_browser);

        // init empty list
        mTitles = new ArrayList<>();
        mUrls = new ArrayList<>();

        // get data from intent
        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            mUrl = bundle.getString(INTENT_EXTRA_URL);
        }

        // inflate layout
        mSwipeRefresh = (CustomSwipeRefreshLayout) findViewById(R.id.swipe_refresh);
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mWebview = (WebView) findViewById(R.id.webview);

        // configure toolbar
        mToolbar = (Toolbar) findViewById(R.id.toolbar);
        mToolbar.setNavigationIcon(R.drawable.ic_baseline_close_24);
        mToolbar.setTitle(mTitle);
        mToolbar.setSubtitle(mUrl);
        setSupportActionBar(mToolbar);
        // toolbar back button
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        }

        // setup listener
        mSwipeRefresh.setCanChildScrollUpCallback(this);
        mSwipeRefresh.setOnRefreshListener(this);

        loadPage(mUrl);
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle item selection
        // back button toolbar
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onBackPressed() {
        // back to previous page
        if (mWebview.canGoBack()) {
            // check list title and url have 1 data minimum
            if (mTitles.size() > 0 && mUrls.size() > 0) {
                // remove last title and url (subtitle) to getting previous one
                mTitles.remove(getLastIndex(mTitles));
                mUrls.remove(getLastIndex(mUrls));
                // change title and subtitle
                changeTitleSubtitle(
                        mTitles.get(getLastIndex(mTitles)),
                        mUrls.get(getLastIndex(mUrls)));

                mWebview.goBack();
            } else {
                // destroy activity
                finish();
            }
        } else {
            // destroy activity
            finish();
        }
    }

    /**
     * Method for loading page from URL
     */
    @SuppressLint("SetJavaScriptEnabled")
    private void loadPage(String url) {
        // validate url
        String validatedUrl = validateUrl(url);
        // set web settings
        WebSettings webSettings = mWebview.getSettings();
        webSettings.setJavaScriptEnabled(true);
        webSettings.setAllowFileAccess(true);
        webSettings.setCacheMode(WebSettings.LOAD_NO_CACHE);
        // enable mixed content
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            webSettings.setMixedContentMode(WebSettings.MIXED_CONTENT_ALWAYS_ALLOW);
        }
        // set webview client
        mWebview.setWebViewClient(new WebViewClient() {
            @SuppressWarnings("deprecation")
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                view.loadUrl(url);
                return true;
            }

            @Override
            public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
                handler.cancel();
            }

            @Override
            public void onPageStarted(WebView view, String url, Bitmap favicon) {
                super.onPageStarted(view, url, favicon);
                // visible progress bar
                mProgress.setVisibility(View.VISIBLE);
            }

            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                // hide progress bar
                mProgress.setVisibility(View.INVISIBLE);
                // finish refreshing
                mSwipeRefresh.setRefreshing(false);
            }
        });

        // set web chrome client
        mWebview.setWebChromeClient(new WebChromeClient() {
            public void onProgressChanged(WebView view, int progress) {
                mProgress.setProgress(progress);
                if (progress >= 100) {
                    // hide progress bar
                    mProgress.setVisibility(View.INVISIBLE);
                    // finish refreshing
                    mSwipeRefresh.setRefreshing(false);
                }
            }

            @Override
            public void onReceivedTitle(WebView view, String title) {
                super.onReceivedTitle(view, title);
                // add title and url (subtitle) to list
                mTitles.add(title);
                mUrls.add(view.getUrl());
                // change title and subtitle
                changeTitleSubtitle(
                        mTitles.get(getLastIndex(mTitles)),
                        mUrls.get(getLastIndex(mUrls)));
            }
        });

        mWebview.loadUrl(validatedUrl);
    }

    // URL validator
    private String validateUrl(String url) {

        String mUrl;

        if (!url.startsWith("http://") && !url.startsWith("https://")) {
            mUrl = "http://" + url;
        } else {
            mUrl = url;
        }

        return mUrl;
    }

    // share menu click
    private void sharePage() {
        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, mTitle);
        shareIntent.putExtra(Intent.EXTRA_TEXT, validateUrl(mUrl));
        shareIntent.setType("text/plain");
        startActivity(shareIntent);
    }

    // open in browser menu click
    private void openInBrowser(String url) {
        Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(validateUrl(url)));
        startActivity(browserIntent);
    }

    // change title and subtitle
    private void changeTitleSubtitle(String title, String url) {
        if (mToolbar != null) {
            mToolbar.setTitle(title);
            mToolbar.setSubtitle(url);
        }
    }

    // get last index of list
    private int getLastIndex(List<String> list) {
        return list.size() - 1;
    }

    /**
     * Interface Implementation
     */

    @Override
    public boolean canSwipeRefreshChildScrollUp() {
        return mWebview.getScrollY() > 0;
    }

    @Override
    public void onRefresh() {
        // reload page
        if (mUrls.size() > 0) {
            loadPage(mUrls.get(getLastIndex(mUrls)));
        } else {
            loadPage(mUrl);
        }
    }

    /**
     * Builder Object Creation Pattern
     */

    static class Builder {

        private Bundle mExtras;

        private Builder() {
            mExtras = new Bundle();
        }

        static Builder getBuilder() {
            return new Builder();
        }

        Builder withUrl(String url) {
            mExtras.putString(INTENT_EXTRA_URL, url);
            return this;
        }

        Intent build(Context ctx) {
            Intent i = new Intent(ctx, InAppBrowserActivity.class);
            i.putExtras(mExtras);
            return i;
        }
    }
}