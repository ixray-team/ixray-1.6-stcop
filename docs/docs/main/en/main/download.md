# How to Download IX-Ray (Stable and Rolling Versions)
## **Overview**

There are two ways to download IX-Ray platform modules (engine, editors, utilities, plugins, etc.):

1.  **Download the stable version from releases** — recommended for most users.
2.  **Download the rolling version** — for those who need the latest changes.

> [!IMPORTANT]
> **Important:** Rolling releases are builds that are assembled after each commit. Such releases may contain bugs, incomplete features, test features, and are unstable. **Only in full releases do you get a fully tested and stable version of IX-Ray.** Full releases lag behind rolling versions because we want to provide a finished product.

### **What is an artifact?**
**Artifact** — is any file or set of files created during the automatic build process of the project. In our case — these are assembled IX-Ray platform files. Artifacts are not stored in the main project code. They are created "on the fly" after each commit. Their main purpose — **to give you the ability to download the latest version of the IX-Ray platform (engine, editors, utils, plugins) without building it yourself.**

---

## **Method 1: Downloading the Stable Version (Recommended)**

1.  Go to the main repository page: **[ixray-1.6-stcop](https://github.com/ixray-team/ixray-1.6-stcop)**
2.  On the main page of the repository, find and click on the **`Releases`** tab.

![Releases tab in the repository](https://github.com/user-attachments/assets/0797277d-6518-4f3d-b335-8ba57e2beae9)

3.  You will go to the page with the latest stable release. Here you can select **`game`** (game) or **`develop`** (for developers, includes editors) build depending on your needs.

![Release page with build selection](https://github.com/user-attachments/assets/a96c10e5-55c8-4816-a64f-53b749bc136f)

4.  Just click on the desired file to download it. This is the simplest and most reliable way.

> [!TIP]
> Registration is not required to download releases!

---

## **Method 2: Downloading the Latest (Rolling) Version via Actions**

### **Step 1: Preparation and Repository Login**

1.  Go to **[github.com](https://github.com)**.
2.  **Log in** (create an account if necessary).  
    *Hint: If you can't select a Russian number (+7) during registration, select Kazakhstan.*
3.  Follow the link to the official repository:  
    **[https://github.com/ixray-team/ixray-1.6-stcop](https://github.com/ixray-team/ixray-1.6-stcop)**
4.  You will be on the main page of the project.
5.  **(Important!)** Be sure to give the repository a `Stars` (star) to support the developers! The button is in the upper right corner of the page.

![Stars button in the repository](https://github.com/user-attachments/assets/4300dd6a-5a74-49fb-8921-217f3a659c88)

### **Step 2: Go to the "Actions" tab**

At the top of the page, below the repository name, there is a menu: **`Code`**, **`Issues`**, **`Pull requests`**, **`Discussions`**, **`Actions`**, etc.

*   Click on the **`Actions`** tab.

![Actions tab](https://github.com/user-attachments/assets/e8d2a9b5-09b8-4edd-9a4a-866e5a37c4ea)

### **Step 3: Search for a successful build**

> [!NOTE] 
> **What do we see?** We got to the section with the history of automatic launches (**workflow**). On the left is a list of these workflows (scripts for building).

Now you need to find the latest successful build. This is the most important step.

1.  In the left sidebar, select a workflow (for example, **`Build engine`**).
2.  Then pay attention to the center list. The most recent builds are at the top.
3.  In the **`Branch`** column, select the branch you need (we recommend using only `default` and `develop`).
4.  Find the top-most run with a **✅ (green checkmark)** in the left column. This is the latest successful build. A red cross (❌) means an error, and a yellow circle (🟡) means the build is in progress.
5.  Click on its name (for example, `Rename debug console commands #8622`).

![List of successful builds](https://github.com/user-attachments/assets/6b7b5554-7d6f-4ade-8849-9427e524c224)

### **Step 4: Downloading the Artifact**

1.  On the run page, scroll down to the **`Artifacts`** section.
2.  In the list, find the artifact named `engine-publish-windows-latest-x64-...`.
3.  Click on the name of the desired artifact to start downloading.

![Artifacts section](https://github.com/user-attachments/assets/5bfa1e82-ae0a-4283-9817-99937d42b1c6)

> [!WARNING]  
> Without authentication you cannot download the artifact!

---

## **Installation**

1.  The downloaded file (for both methods) is a ZIP archive
2.  **Extract its contents to the folder with the installed game**
3.  When asked to **replace files** — agree
4. Unpack original *.db files
> [!NOTE] 
> **Key Warning:** IX-Ray is only an **engine**. It requires a pre-installed legal copy of the original game.

## **Summary**

*   **For a stable game:** use **Releases**
*   **To test the latest features:** use **Actions** (artifacts)
*   **Don't forget to support the project with a star (`Stars`)!**
