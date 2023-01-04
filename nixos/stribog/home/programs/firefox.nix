{pkgs, ...}: {
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      i-dont-care-about-cookies
      ublock-origin
      youtube-recommended-videos
    ];
    profiles = {
      default = {
        bookmarks = [
          {
            keyword = ":bc";
            name = "bitchute";
            url = "https://www.bitchute.com/search/?query=%s";
          }
          {
            keyword = ":d";
            name = "duckduckgo";
            url = "https://duckduckgo.com/?q=%s&kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l";
          }
          {
            keyword = ":gh";
            name = "github";
            url = "https://github.com/search?q=%s";
          }
          {
            keyword = ":gtr";
            name = "google-translate-to-russian";
            url = "https://translate.google.com/?sl=auto&tl=ru&op=translate&text=%s";
          }
          {
            keyword = ":gte";
            name = "google-translate-to-english";
            url = "https://translate.google.com/?sl=auto&tl=en&op=translate&text=%s";
          }
          {
            keyword = ":gti";
            name = "google-translate-to-italian";
            url = "https://translate.google.com/?sl=auto&tl=it&op=translate&text=%s";
          }
          {
            keyword = ":maband";
            name = "metalarchivesband";
            url = "https://www.metal-archives.com/search?searchString=%s&type=band_name";
          }
          {
            keyword = ":os";
            name = "odysee";
            url = "https://odysee.com/$/search?q=%s";
          }
          {
            keyword = ":rt";
            name = "rutracker";
            url = "https://rutracker.net/forum/tracker.php?nm=%s";
          }
          {
            keyword = ":sp";
            name = "startpage";
            url = "https://startpage.com/sp/search?query=%s&prfe=a715a36c09c1472e9d5d804b0ba9312716a96d474575edbfa5e7cb0c646b34216e65fa4ae420b5df58e6c8d3e420eb1771f23caa2663bb5435b01ebb741af66083a80b0bb3682e008b0e7e1126";
          }
          {
            keyword = ":sx";
            name = "searx";
            url = "https://searx.be/?q=%s&preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==";
          }
          {
            keyword = ":ya";
            name = "yandex";
            url = "https://yandex.ru/search/?text=%s";
          }
          {
            keyword = ":yt";
            name = "youtube";
            url = "https://www.youtube.com/results?search_query=%s";
          }
        ];

        settings = {
          "accessibility.typeaheadfind" = false;
          "accessibility.typeaheadfind.flashBar" = 0;
          "app.normandy.first_run" = false;
          "app.shield.optoutstudies.enabled" = false;
          "app.update.auto" = false;
          "breakpad.reportURL" = "";
          "browser.bookmarks.restore_default_bookmarks" = false;
          "browser.discovery.enabled" = false;
          "browser.download.always_ask_before_handling_new_types" = true;
          "browser.download.dir" = "/tmp";
          "browser.download.folderList" = 2;
          "browser.download.panel.show" = true;
          "browser.download.panel.shown" = true;
          "browser.download.useDownloadDir" = false;
          "browser.download.viewableInternally.typeWasRegistered.avif" = true;
          "browser.download.viewableInternally.typeWasRegistered.webp" = true;
          "browser.formfill.enable" = false;
          "browser.laterrun.enabled" = true;
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;
          "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "";
          "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "";
          "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
          "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
          "browser.newtabpage.activity-stream.showSearch" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.enabled" = false;
          "browser.newtabpage.pinned" = "[]";
          "browser.safebrowsing.downloads.enabled" = false;
          "browser.safebrowsing.malware.enabled" = false;
          "browser.safebrowsing.phishing.enabled" = false;
          "browser.search.hiddenOneOffs" = "Google,Amazon.com,Bing,DuckDuckGo,eBay,Wikipedia (en)";
          "browser.search.region" = "US";
          "browser.search.suggest.enabled" = false;
          "browser.shell.checkDefaultBrowser" = false;
          "browser.startup.homepage" = "about:blank";
          "browser.startup.homepage_override.mstone" = "ignore";
          "browser.tabs.crashReporting.sendReport" = false;
          "browser.tabs.firefox-view" = false;
          "browser.toolbars.bookmarks.visibility" = "never";
          "browser.uiCustomization.state" = "{\"placements\":{\"widget-overflow-fixed-list\":[],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"urlbar-container\",\"downloads-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\",\"myallychou_gmail_com-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"new-tab-button\",\"alltabs-button\"],\"PersonalToolbar\":[\"import-button\",\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\",\"myallychou_gmail_com-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"PersonalToolbar\",\"toolbar-menubar\",\"TabsToolbar\"],\"currentVersion\":17,\"newElementCount\":3}";
          "browser.urlbar.placeholderName.private" = "";
          "browser.urlbar.quicksuggest.scenario" = "offline";
          "browser.urlbar.resultGroups" = "{\"children\":[{\"maxResultCount\":1,\"children\":[{\"group\":\"heuristicTest\"},{\"group\":\"heuristicExtension\"},{\"group\":\"heuristicSearchTip\"},{\"group\":\"heuristicOmnibox\"},{\"group\":\"heuristicEngineAlias\"},{\"group\":\"heuristicBookmarkKeyword\"},{\"group\":\"heuristicAutofill\"},{\"group\":\"heuristicPreloaded\"},{\"group\":\"heuristicTokenAliasEngine\"},{\"group\":\"heuristicFallback\"}]},{\"group\":\"extension\",\"availableSpan\":5},{\"flexChildren\":true,\"children\":[{\"group\":\"generalParent\",\"children\":[{\"availableSpan\":3,\"group\":\"inputHistory\"},{\"flexChildren\":true,\"children\":[{\"flex\":1,\"group\":\"remoteTab\"},{\"flex\":2,\"group\":\"general\"},{\"flex\":2,\"group\":\"aboutPages\"},{\"flex\":1,\"group\":\"preloaded\"}]},{\"group\":\"inputHistory\"}],\"flex\":2},{\"children\":[{\"flexChildren\":true,\"children\":[{\"flex\":2,\"group\":\"formHistory\"},{\"flex\":4,\"group\":\"remoteSuggestion\"}]},{\"group\":\"tailSuggestion\"}],\"flex\":1}]}]}";
          "browser.urlbar.shortcuts.bookmarks" = false;
          "browser.urlbar.shortcuts.history" = false;
          "browser.urlbar.shortcuts.tabs" = false;
          "browser.urlbar.showSearchSuggestionsFirst" = false;
          "browser.urlbar.suggest.bookmark" = false;
          "browser.urlbar.suggest.engines" = false;
          "browser.urlbar.suggest.history" = false;
          "browser.urlbar.suggest.openpage" = false;
          "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
          "browser.urlbar.suggest.quicksuggest.sponsored" = false;
          "browser.urlbar.suggest.searches" = false;
          "browser.urlbar.suggest.topsites" = false;
          "datareporting.healthreport.documentServerURI" = "";
          "datareporting.healthreport.service.enabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "doh-rollout.skipHeuristicsCheck" = true;
          "dom.battery.enabled" = false;
          "dom.event.clipboardevents.enabled" = false;
          "dom.forms.autocomplete.formautofill" = false;
          "dom.ipc.plugins.flash.subprocess.crashreporter.enabled" = false;
          "dom.ipc.plugins.reportCrashURL" = false;
          "extensions.formautofill.addresses.enabled" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "extensions.pictureinpicture.enable_picture_in_picture_overrides" = false;
          "extensions.pocket.enabled" = false;
          "extensions.screenshots.disabled" = true;
          "extensions.ui.dictionary.hidden" = true;
          "extensions.ui.locale.hidden" = true;
          "extensions.ui.sitepermission.hidden" = true;
          "extensions.webextensions.ExtensionStorageIDB.migrated.jid1-KKzOGWgsW3Ao4Q@jetpack" = true;
          "extensions.webextensions.ExtensionStorageIDB.migrated.uBlock0@raymondhill.net" = true;
          "extensions.webextensions.uuids" = "{\"uBlock0@raymondhill.net\":\"ed81545f-4149-47b9-a289-ea98eb20221d\",\"jid1-KKzOGWgsW3Ao4Q@jetpack\":\"38073834-2697-45b0-8238-c233089a7f8f\",\"doh-rollout@mozilla.org\":\"3e2f9e29-6753-4af0-87f4-30a67b9f1102\",\"formautofill@mozilla.org\":\"9b145aac-9bb4-4196-be4f-3cd25eefdb90\",\"pictureinpicture@mozilla.org\":\"4dc52942-8a8e-4aae-b312-6bbfd49aff0f\",\"screenshots@mozilla.org\":\"1deb33d4-528a-4034-9345-6c48cf23b301\",\"webcompat-reporter@mozilla.org\":\"dff469c9-d240-4a37-862f-a5b44fdf8b7c\",\"webcompat@mozilla.org\":\"82123520-5397-4573-9baa-f027fc401435\",\"default-theme@mozilla.org\":\"5296f9c7-d307-4aaf-b60a-406d4e0aa56f\",\"addons-search-detection@mozilla.com\":\"186e74f8-d5de-4409-a45a-55affa3faebb\",\"google@search.mozilla.org\":\"147e8d5f-207a-4397-8d05-11e68efdfd4a\",\"wikipedia@search.mozilla.org\":\"2faa6d11-eb7a-46bf-914f-663f5715b06e\",\"bing@search.mozilla.org\":\"fbc35289-8926-4380-ae6d-4b59984e28f8\",\"ddg@search.mozilla.org\":\"8624cf23-6a11-44c5-9143-26cb22b3fcc4\",\"amazon@search.mozilla.org\":\"86ed2a09-1db8-4add-bef7-b20dbea8621a\",\"amazondotcom@search.mozilla.org\":\"d4c17cbd-5aa0-4a57-885c-c484be3f138f\",\"myallychou@gmail.com\":\"4c2a7221-8620-40b5-9b25-572992d6e67f\",\"ebay@search.mozilla.org\":\"7016e5e1-271c-4e94-b611-ecbf00c5d9af\"}";

          # Breaks google meet
          # "general.platform.override" = "Win32";
          # "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; rv:91.0) Gecko/20100101 Firefox/91.0";

          "geo.enabled" = false;
          "geo.wifi.logging.enabled" = false;
          "geo.wifi.uri" = "";
          "identity.fxaccounts.auth.uri" = "";
          "identity.fxaccounts.enabled" = false;
          "identity.fxaccounts.remote.force_auth.uri" = "";
          "identity.fxaccounts.remote.signin.uri" = "";
          "identity.fxaccounts.remote.signup.uri" = "";
          "identity.fxaccounts.settings.uri" = "";
          "layout.css.prefers-color-scheme.content-override" = 2;
          "layout.spellcheckDefault" = 0;
          "loop.enabled" = false;
          "media.autoplay.default" = 5;
          "media.hardwaremediakeys.enabled" = false;
          "media.navigator.enabled" = false;
          "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
          "network.captive-portal-service.enabled" = false;
          "network.dns.disablePrefetch" = true;
          "network.http.speculative-parallel-limit" = 0;
          "network.predictor.enabled" = false;
          "network.prefetch-next" = false;
          "pdfjs.enabledCache.state" = false;
          "permissions.default.desktop-notification" = 2;
          "permissions.default.geo" = 2;
          "permissions.default.xr" = 2;
          "places.history.enabled" = false;
          "pref.downloads.disable_button.edit_actions" = false;
          "privacy.clearOnShutdown.cache" = true;
          "privacy.clearOnShutdown.cookies" = true;
          "privacy.clearOnShutdown.downloads" = true;
          "privacy.clearOnShutdown.formdata" = true;
          "privacy.clearOnShutdown.history" = true;
          "privacy.clearOnShutdown.offlineApps" = true;
          "privacy.clearOnShutdown.openWindows" = true;
          "privacy.clearOnShutdown.passwords" = true;
          "privacy.clearOnShutdown.sessions" = true;
          "privacy.clearOnShutdown.siteSettings" = true;
          "privacy.donottrackheader.enabled" = true;
          "privacy.donottrackheader.value" = 1;
          "privacy.firstparty.isolate" = true;
          "privacy.history.custom" = true;
          "privacy.sanitize.pending" = "[{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}},{\"id\":\"shutdown\",\"itemsToClear\":[\"cache\",\"cookies\",\"offlineApps\",\"history\",\"formdata\",\"downloads\",\"sessions\",\"siteSettings\"],\"options\":{}}]";
          "privacy.sanitize.sanitizeOnShutdown" = true;
          "privacy.trackingprotection.enabled" = true;
          "services.sync.autoconnect" = false;
          "services.sync.clients.lastSync" = "0";
          "services.sync.declinedEngines" = "";
          "services.sync.engine.addons" = false;
          "services.sync.engine.addresses.available" = false;
          "services.sync.engine.bookmarks" = false;
          "services.sync.engine.history" = false;
          "services.sync.engine.passwords" = false;
          "services.sync.engine.prefs" = false;
          "services.sync.engine.tabs" = false;
          "services.sync.globalScore" = 0;
          "services.sync.nextSync" = 0;
          "services.sync.serverURL" = "";
          "services.sync.tabs.lastSync" = "0";
          "signon.autofillForms" = false;
          "signon.generation.enabled" = false;
          "signon.management.page.breach-alerts.enabled" = false;
          "signon.rememberSignons" = false;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.cachedClientID" = "";
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.optoutSample" = false;
          "toolkit.telemetry.pioneer-new-studies-available" = false;
          "toolkit.telemetry.prompted" = 2;
          "toolkit.telemetry.rejected" = true;
          "toolkit.telemetry.reportingpolicy.firstRun" = false;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.unifiedIsOptIn" = true;
        };
      };
    };
  };
}
