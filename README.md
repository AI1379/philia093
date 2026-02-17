# Philia093 Bot

> ![](assets/cyrene.png)
>
> 嗨，想我了吗？

这是一个新的小机器人，功能主要是自动从一些平台爬取信息流，接入 LLM 接口摘要后生成总结文章，通过邮件等方式推送到手机。

这个项目目前用 Haskell 开发~~因为闲着无聊~~。目前还在开发阶段，暂不可用。如果有一天懒得维护了可能会用 Python 重新实现。

## Features

- Notify
  - [ ] SMTP 邮件发送通知
  - [ ] OneBot 的 HTTP WebHook API 通知
  - [ ] 钉钉官方机器人的 WebHook 通知
- 信息源
  - [ ] arXiv 邮件订阅
  - [ ] RSS 订阅
- LLM / Agent
  - [x] 基本 LLM 功能
  - [ ] LLM Tool Calling
  - [ ] 接入 OpenClaw
- 其他功能
  - [ ] 邮件修改订阅列表

> **这个项目和[纳西妲](https://github.com/AI1379/nahida-bot)有什么区别和关系吗？**
>
> 没有。显而易见的，这两个项目的技术栈都不一样。Philia093 更倾向于单纯的信息流推送，而 `nahida-bot` 会做一些语言交互性的内容。
