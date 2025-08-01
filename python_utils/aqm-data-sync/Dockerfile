FROM continuumio/miniconda3

RUN apt-get update --yes && apt-get install --yes tmux vim less

COPY environment.yml /opt/build/environment.yml
RUN conda env create -f /opt/build/environment.yml

COPY pyproject.toml /opt/build/pyproject.toml
COPY src /opt/build/src
WORKDIR /opt/build
RUN conda run -n aqm-data-sync pip install .

RUN rm -rf /opt/build
