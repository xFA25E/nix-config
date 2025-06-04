{pkgs, ...}: {
  # boot.kernelParams = ["usbcore.autosuspend=-1"];
  environment.systemPackages = [pkgs.rnnoise pkgs.helvum pkgs.qpwgraph];

  services.pipewire = {
    alsa = {
      enable = true;
      support32Bit = true;
    };

    enable = true;

    extraConfig = {
      client."10-resample-quality"."context.properties"."resample.quality" = 4;
      pipewire."99-input-denoising"."context.modules" = [
        {
          "name" = "libpipewire-module-filter-chain";
          "args" = {
            "node.description" = "Noise Canceling source";
            "media.name" = "Noise Canceling source";
            "filter.graph" = {
              "nodes" = [
                {
                  "type" = "ladspa";
                  "name" = "rnnoise";
                  "plugin" = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                  "label" = "noise_suppressor_mono";
                  "control" = {
                    "VAD Threshold (%)" = 50.0;
                    "VAD Grace Period (ms)" = 200;
                    "Retroactive VAD Grace (ms)" = 0;
                  };
                }
                # {
                #   "name" = "gain";
                #   "type" = "ladspa";
                #   "plugin" = "gain";
                #   "label" = "volume";
                #   "control" = {
                #     "Gain (dB)" = 10.0;
                #   };
                # }
              ];
            };
            "capture.props" = {
              "node.name" = "capture.rnnoise_source";
              "node.passive" = true;
              "audio.rate" = 48000;
            };
            "playback.props" = {
              "node.name" = "rnnoise_source";
              "media.class" = "Audio/Source";
              "audio.rate" = 48000;
            };
          };
        }
      ];
    };

    jack.enable = true;
    pulse.enable = true;
  };

  security.rtkit.enable = true;
}
