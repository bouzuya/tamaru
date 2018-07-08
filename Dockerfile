FROM node:10
RUN useradd -m myuser
USER myuser
WORKDIR /home/myuser
COPY --chown=myuser:myuser . .
RUN echo '{"interactive":false}' > .bowerrc
RUN npm install
CMD ["npm", "start"]
