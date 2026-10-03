import { defineConfig } from "vitepress";
import lightbox from "vitepress-plugin-lightbox";
import { enLocale } from "./locales/en.mts";
import { ruLocale } from "./locales/ru.mts";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  groupIconMdPlugin,
  groupIconVitePlugin,
} from "vitepress-plugin-group-icons";
import llmstxt from "vitepress-plugin-llms";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const ltxGrammar = JSON.parse(
  fs.readFileSync(path.join(__dirname, "shiki", "ltx.tmLanguage.json"), "utf8"),
);

const siteTitle = "IX-Ray Platform";
const siteDescription =
  "Официальная страница проекта IX-Ray Platform. https://github.com/ixray-team/ixray-1.6-stcop";
const siteUrl = "https://ixray-team.github.io/ixray-1.6-stcop";
const ogImage = `${siteUrl}/logo.svg`;

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: siteTitle,
  description: siteDescription,

  base: "/ixray-1.6-stcop/",
  srcDir: "../docs",
  outDir: "../public",
  lastUpdated: true,
  ignoreDeadLinks: true,
  rewrites: {
    "main/ru/:rest*": ":rest*",
    "main/en/:rest*": "en/:rest*",
    "mods/ru/:rest*": "mods/:rest*",
    "mods/en/:rest*": "en/mods/:rest*",
  },
  sitemap: {
    hostname: "https://ixray-team.github.io/ixray-1.6-stcop",
  },
  head: [
    ["link", { rel: "icon", href: "./favicon.ico" }],

    // Основные метатеги
    ["meta", { name: "description", content: siteDescription }],
    [
      "meta",
      {
        name: "keywords",
        content: "IX-Ray, IX-Ray Platform, S.T.A.L.K.E.R.",
      },
    ],
    ["meta", { name: "author", content: "IX-Ray Team" }],
    ["meta", { name: "robots", content: "index, follow" }],
    ["meta", { name: "theme-color", content: "#1a1a1a" }],

    // Open Graph
    ["meta", { property: "og:type", content: "website" }],
    ["meta", { property: "og:site_name", content: siteTitle }],
    ["meta", { property: "og:title", content: siteTitle }],
    ["meta", { property: "og:description", content: siteDescription }],
    ["meta", { property: "og:url", content: siteUrl }],
    ["meta", { property: "og:image", content: ogImage }],
    ["meta", { property: "og:image:alt", content: siteTitle }],
    ["meta", { property: "og:locale", content: "ru_RU" }],
    ["meta", { property: "og:locale:alternate", content: "en_US" }],

    // Twitter Card
    ["meta", { name: "twitter:card", content: "summary_large_image" }],
    ["meta", { name: "twitter:title", content: siteTitle }],
    ["meta", { name: "twitter:description", content: siteDescription }],
    ["meta", { name: "twitter:image", content: ogImage }],
    ["meta", { name: "twitter:image:alt", content: siteTitle }],
  ],

  locales: {
    root: ruLocale,
    en: enLocale,
  },
  markdown: {
    container: {
      customContainers: {
        success: "SUCCESS",
      },
    },
    config: (md) => {
      md.use(lightbox, {});
      md.use(groupIconMdPlugin);
    },
    async shikiSetup(highlighter) {
      console.log("Loading LTX grammar...", ltxGrammar.name);
      await highlighter.loadLanguage(ltxGrammar);
      console.log(
        "LTX loaded:",
        highlighter.getLoadedLanguages().includes("ltx"),
      );
    },
  },

  vite: {
    plugins: [groupIconVitePlugin(), llmstxt()],
  },

  themeConfig: {
    logo: "/logo.svg",
    search: {
      provider: "local",
      options: {},
    },
    socialLinks: [
      { icon: "github", link: "https://github.com/ixray-team/ixray-1.6-stcop" },
      { icon: "discord", link: "https://discord.gg/hWTbHxaYWz" },
      { icon: "telegram", link: "https://t.me/ixray_platform" },
    ],
  },
});
