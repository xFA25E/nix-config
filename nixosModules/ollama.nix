{pkgs, ...}: {
  services.ollama = {
    enable = true;
    loadModels = ["qwen2.5-coder:7b" "deepseek-r1:8b"];
    package = pkgs.ollama-cuda;
    environmentVariables = {
      OLLAMA_KEEP_ALIVE = "24h";
    };
  };
}
