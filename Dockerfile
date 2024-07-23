FROM node:20.10.0 AS elm-build

WORKDIR /app

RUN npm install yarn
COPY elm-tooling.json elm.json package.json vite.config.js yarn.lock /app/
RUN yarn install --dev

COPY index.html main.js style.css /app/
COPY src/ /app/src/

RUN yarn run vite build


FROM nginx
COPY --from=elm-build /app/dist/assets/* /usr/share/nginx/html/assets/
COPY --from=elm-build /app/dist/index.html /usr/share/nginx/html/index.html
