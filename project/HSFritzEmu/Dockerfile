FROM haskell:latest as build
WORKDIR /build
COPY . .
RUN stack build --install-ghc && cp $(find -type f -name "MateEmu" | tail -n1) .

FROM ubuntu:latest
EXPOSE 8080
EXPOSE 1337/udp
COPY --from=build /build/MateEmu /usr/local/bin
ENTRYPOINT ["MateEmu"]
CMD ["--mip=0.0.0.0"]
