### Установка docker
- https://docs.docker.com/docker-for-windows/install/

## Краткая инструкция
### Директория исходного кода
Директория ваших исходных файлов для запуска в контейнере
- папка code
### Зайти в контейнер (bash):
  1) закомментировать **command:** **["./make_and_run.sh","gfortran L1T2.F95 -o exec","./exec"]** в **docker-compose.yml`**
  2) в командной строке ввести `docker-compose up`
  3) в командной строке ввести `docker exec -it -u root fortran bash`
### Собрать и запустить в контейнере
  1) Перейти в папку с исходным файлом(пример файла L1T2.F95) `cd /fortran`
  2) Скомпилировать проект `gfortran L1T2.F95 -o exec` , где (исходный файл - L1T2.F95) (exec - имя бинарника на выходе)
  3) Запустить бинарник `./exec`
  
### Остановить и очистить контейнер 
- Ctrl+C = stop 
- docker-compose down = stop and clear

### Удалить образы
- clear_images.bat (windows)
