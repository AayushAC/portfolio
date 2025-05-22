use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::{get, post},
    Json, Router,
};
use dotenv::dotenv;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, env, net::SocketAddr};

#[tokio::main]
async fn main() {
    dotenv().ok();

    let app = Router::new()
        .route("/hello", get(hello_handler))
        .route("/greet/:name", get(greet_handler))
        .route("/greetme", post(greet_me_handler))
        .route("/announcements", get(announcements_handler));

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    println!("Server running on http://{}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

// ========== Data Models ==========

#[derive(Serialize)]
struct GreetResponse {
    greet: String,
    name: String,
}

#[derive(Deserialize)]
struct GreetMeInput {
    input: String,
    name: String,
}

#[derive(Serialize)]
struct MsgResponse {
    msg: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Announcement {
    title: String,
    description: String,
    created_at: String,
}

// ========== Handlers ==========

async fn hello_handler() -> &'static str {
    "Hello, World!"
}

async fn greet_handler(Path(name): Path<String>) -> Json<GreetResponse> {
    Json(GreetResponse {
        greet: "Hello".to_string(),
        name,
    })
}

async fn greet_me_handler(Json(payload): Json<GreetMeInput>) -> Json<MsgResponse> {
    let msg = format!("{} {}", payload.input, payload.name);
    Json(MsgResponse { msg })
}

// JSON or HTML response switch
async fn announcements_handler(Query(params): Query<HashMap<String, String>>) -> impl IntoResponse {
    match fetch_announcements().await {
        Ok(announcements) => {
            let format = params.get("format").map(|s| s.as_str()).unwrap_or("json");
            match format {
                "html" => {
                    let html = format_html(&announcements);
                    Html(html).into_response()
                }
                _ => Json(announcements).into_response(),
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Failed to fetch announcements").into_response()
        }
    }
}

// ========== Fetch & Format ==========

async fn fetch_announcements() -> Result<Vec<Announcement>, reqwest::Error> {
    let url = env::var("GITLAB_API_URL")
        .unwrap_or_else(|_| "https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened".to_string());

    let token = env::var("GITLAB_TOKEN").expect("GITLAB_TOKEN is not set in .env");

    let client = reqwest::Client::new();
    let res = client
        .get(url)
        .header("PRIVATE-TOKEN", token)
        .send()
        .await?
        .json::<Vec<Announcement>>()
        .await?;

    let mut sorted = res;
    sorted.sort_by(|a, b| a.created_at.cmp(&b.created_at));
    Ok(sorted)
}

fn format_html(announcements: &[Announcement]) -> String {
    let mut html = String::from("<ul>");
    for ann in announcements {
        let item = format!(
            "<li><strong>{}</strong>: {} <em>({})</em></li>",
            ann.title, ann.description, ann.created_at
        );
        html.push_str(&item);
    }
    html.push_str("</ul>");
    html
}
