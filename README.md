# 🪙 Developers' Wages API 💻

[![Docker Image CI](https://github.com/stesiam/Dev-Wages-Api/actions/workflows/docker-image.yml/badge.svg)](https://github.com/stesiam/Dev-Wages-Api/actions/workflows/docker-image.yml)

A [plumber](https://www.rplumber.io/) API which is based on the recently conducted survey of Greek Developers'.



## Main Endpoints

- Predict Wages 
- Predict Wages (without specifiying gender)
- Gender Wage Gap (Difference in expected payment, Wage Men - Wage Women)

---

## Get & Run Image

You can get the image by running the following command in your terminal:

```bash
docker pull stesiam/dev-wages-api
```

To run the image:

```bash
docker run -p 8000:8000 stesiam/dev-wages-api
```

When you run the image, the API documentation will be available at <a href="http://127.0.0.1:8000/__docs__/">http://127.0.0.1:8000/__docs__/</a>


## Links

📦 **DockerHub Repo**: [stesiam/dev-wages-api](https://hub.docker.com/repository/docker/stesiam/dev-wages-api) <br>
💾 **Data Source**: [Social Nerds](https://www.youtube.com/channel/UCd5jW000te6bExqYth4TIxQ)
