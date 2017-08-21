# Инфраструктура: OTP фреймворк, rebar, релизы

OTP фреймворк важная часть эрланг. Настолько важная, что язык на самом
деле называется не просто Erlang, а Erlang/OTP. Проекты на эрланг без
использования OTP встречаются крайне редко, и, обычно, это небольшие
библиотеки.

Рассмотренные на предыдущих уроках gen\_server, supervisor и
application являются частью OTP.

OTP -- это аббревиатура, которая читается как Open Telecom Platform.
То есть, платформа для создания проектов в области Telecom.  Название
не совсем правильное, на самом деле это платформа общего назначения,
для создания любых проектов.

OTP включает:
- приложения, формирующие базовое окружение: kernel, stdlib, erts, sasl;
- другие приложения, реализующие полезные функции: crypto, ssh, ssl, mnesia, wx, xmerl и другие;
- поведения: gen\_server, supervisor, application, gen\_fsm, gen\_event;
- ряд библиотек: lists, dict, maps, rand, re, timer и другие;
- инструменты для работы над проектом: компилятор, отладчик, профилировщик, статический анализатор и другие.

За 20 лет своего существования OTP проверен во многих
высоко-нагруженных и распределенных проектах. Так что гораздо лучше
полагаться на уже имеющиеся средства: gen\_server, supervisor,
библиотеки и т.д., чем пытаться реализовать свои аналогичные.


## Типичная структура проекта

Для эрланг есть рекомендуемая структура проекта. На эту структуру рассчитаны
инструменты, входящие в состав OTP. В первую очередь компилятор, и скрипты,
собирающие релизы.


### Минимальная структура

```
├─ ebin
│  ├── my_app.app
│  └── my_app.beam
└─ src
   └── my_app.erl
```

Самый простой OTP проект может иметь только две папки:
**src** для исходников и **ebin** для скомпилированных модулей.
В ebin также находится и файл ресурсов приложения.


### Проект из одного приложения

```
├─ ebin
│  ├── my_app.app
│  └── my_app.beam
├─ include
│  └── my_app.hrl
├─ priv
│  └── some.resource
└─ src
   └── my_app.erl
```

Кроме этого к стандартным папкам еще относятся:
**include** для заголовочных файлов,
**priv** для хранения каких-либо ресурсов, необходимых проекту
(файлы с данными, ssl-сертификаты, схемы валидации и т.д.).


### Проект с использованием rebar

Популярный инструмент для управления зависимостями и сборки проекта
**rebar** добавляет к этой структуре еще несколько папок.  Они не
упоминаются в рекомендациях OTP, но часто встречаются в проектах, где
используется rebar.

```
├─ deps
├─ ebin
├─ .eunit
├─ include
├─ logs
├─ priv
├─ src
└─ test
```

Тут появляется папка **deps**, куда rebar скачивает зависимости.
И 3 папки, связанные с тестированием:
**test** для хранения тестов (eunit и common test),
**.eunit**, куда складываются скомпилированные модули и тесты,
и **logs** куда складываются отчеты.

Также в корне проекта появляется файл **rebar.config**, с описанием
параметров сборки, зависимостей и прочего. (Подробнее о rebar
и его конфигурации ниже).

Ну и хорошим тоном считается иметь в корне проекта файл **README**,
где кратко описана суть проекта.


### Проект из нескольких приложений

В более сложном случае проект может состоять из нескольких приложений.
И тогда структура его меняется

```
├─ deps
├─ apps
│  ├─ app1
│  │  ├── ebin
│  │  ├── include
│  │  ├── logs
│  │  ├── priv
│  │  ├── src
│  │  └── test
│  ├─ app2
│  │  ├── ebin
│  │  ├── include
│  │  ├── logs
│  │  ├── priv
│  │  ├── src
│  │  └── test
│  └─ appN
│     ├── ebin
│     ├── include
│     ├── logs
│     ├── priv
│     ├── src
│     └── test
├── rebar.config
└── README
```

Каждое приложение имеет свой собственный набор папок, кроме **deps**.
**deps** остается одна на весь проект.


### Еще в проекте могут быть


#### Конфигурационные файлы

Стандарты OTP не определяют, где должны быть конфигурационные
файлы. Они могут быть в корне проекта, или в папке priv.
Чаще в корне проекта.


#### Исходники на других языках

Не редко проекты пишутся с использованием сразу нескольких языков
программирования.  И эрланг-проекты тоже могут иметь исходники на
других языках.  Стандарта на этот счет нет, но есть сложившаяся
практика помещать такие исходники в папки **c_src**, **cpp_src**,
**python_src** и т.д. на одном уровне с папкой **src**, то есть, на
уровне приложения.


#### Файлы веб-сервера

Проект на эрланг может быть веб-проектом. И тогда нужно где-то
хранить веб-ресурсы: html, js, css файлы и графику.

OTP предлагает хранить ресурсы в папке **priv**. Но обычно корневым
каталогом для веб-сервера делают не саму папку **priv**, а
какую-нибудь вложенную:

```
└─ priv
   └─ www
      ├─ css
      ├─ img
      ├─ js
      └─ index.html
```


## rebar

[rebar](https://github.com/rebar/rebar) не является частью Erlang/OTP,
это отдельный проект созданный группой энтузиастов. Он не идеален, но
во многом упрощает работу с эрланг проектом. На сегодняшний день rebar
является стандартом де-факто.  Конкурирует с ним только
[erlang.mk](https://github.com/ninenines/erlang.mk), который
используется гораздо реже.

rebar умеет:

 - собирать проект;
 - генерировать исходники из шаблонов;
 - управлять зависимостями;
 - запускать тесты;
 - генерировать документацию.

Для начала нам хватит 4-х основных команд:

 - **rebar get-deps** -- чтобы скачать зависимые библиотеки;
 - **rebar compile** -- чтобы собрать проект;
 - **rebar eunit** -- чтобы запустить юнит-тесты;
 - **rebar clean** -- чтобы очистить проект.

Сборка инкрементальная, то есть, rebar сравнивает дату модификации
исходного модуля и скомпилированного beam-файла, и пересобирает только
те модули, которые изменились. Но можно еще ускорить сборку проекта,
если указать опцию **skip_deps=true**.

```
rebar compile skip_deps=true
```

При этом rebar не будет собирать зависимые библиотеки и проверять,
были ли в них изменения. Обычно эти библиотеки собираются один раз,
после скачивания, и больше их трогать не нужно.

Так же актуальны

```
rebar eunit skip_deps=true
```

если мы хотим запускать только свои юнит-тесты, а не тесты, имеющиеся
в зависимых библиотеках, и

```
rebar clean skip_deps=true
```

если мы хотим очистить только свои собранные модули, а не собранные
зависимые библиотеки.

Если rebar находит в проекте файлы вида **src/some.app.src**, он
использует их как шаблоны для генерации файлов ресурсов приложения
**ebin/some.app**. rebar подставляет в шаблон список всех модулей,
имеющихся в **src**. Это упрощает поддержку файла ресурсов.


### rebar.config

rebar.config -- это файл в корне проекта с настройками для rebar.
Есть много разных настроек, но большинство из них имеют разумные
значения по умолчанию.  Но некоторые все же необходимо указать явно.
В первую очередь -- зависимости проекта.

Зависимости указываются кортежем вида:

```erlang
{deps, [Dependency1, Dependency2, Dependency3]}
```

где Dependency -- это тоже кортеж вида:

```erlang
{ApplicationName, Version, Source}
```

ApplicationName -- атом, имя приложения.
Version -- строка, версия приложения.
Source -- кортеж вида:

```erlang
{VCS, Url, Revision}
```

VCS -- атом **git** или **hg** или **bzr**, обозначающий систему контроля версий.
Url -- ссылка, откуда можно сказать зависимость.
Revision -- тэг, ветка или коммит, которые нужно скачивать.

Пример:

```erlang
{deps, [
        {pooler, ".*", {git, "git://github.com/seth/pooler.git", "b6c522a67a1d067122705ef725535a8664dd8514"}},
        {epgsql, ".*", {git, "git://github.com/epgsql/epgsql.git", {tag, "3.1.0"}}}
       ]}.
```

Здесь указаны две зависимости, библиотеки **pooler** и **epgsql**.
Источник -- git репозиторий на github.com. В первом случае
указан ID коммита. Во втором случае указан тэг.

Версия приложения обычно не используется, потому что Revision
выполняет ту же роль.  Часто версию просто обозначают как ".*",
то есть "любая".

Кроме зависимостей, еще часто указывают опции для компилятора.

```erlang
{erl_opts, [debug_info,
            warn_missing_spec,
            warning_as_errors
           ]}.
```

Их тоже много разных, но лишь немногие указываются часто.

**debug_info** добавляет отладочную информацию в beam файлы,
**warn_missing_spec** требует, чтобы для всех функции в проекте был указан spec,
**warning_as_errors** предупреждения компилятора интерпретирует как ошибку, то есть, останавливает сборку.


## Проблемы в управлении зависимостями

Часто в проектах на github можно видеть rebar.config, где Revision для
зависимостей указан как "HEAD" или "master" или "". Все эти варианты
значат одно и то же -- брать самую последнюю версию.

Так делать не рекомендуется. Пока вы работаете над своим проектом,
авторы библиотек не дремлют, и тоже работают с ними. Они могут
изменить API или реализацию библиотеки.  И может оказаться, что ваш
код, прекрасно работавший вчера, сегодня уже не работает.  Еще хуже,
если это выяснится уже после разворачивания на боевом сервере.
Поэтому рекомендуется всегда указывать тэг или коммит для всех
зависимостей, которые вы используете.

К сожалению, на этом проблемы не заканчиваются. Еще существуют
транзитивные зависимости.  Например, вы используете библиотеку А, а та
использует библиотеку Б.  Для библиотеки А вы в своем rebar.config
укажете тэг. Но может оказаться, что в rebar.config библиотеки А
указана зависимость от Б без тэга.

И тут нет простого выхода из ситуации. Либо жить с этим и надеяться на
лучшее, что годится для любительского проекта. Либо делать форк
библиотеки А и исправлять в ней rebar.config. Для серьезного проекта
вполне разумно сделать форки всех зависимостей, в том числе
транзитивных.

Далее проблемы рождают уже сделанные форки. Оригинальные библиотеки
развиваются авторами дальше -- в них исправляются баги, добавляются новые
функции.  Вам нужно либо отслеживать эти изменения и мержить их в
свои форки, либо игнорировать их.


## Релизы

Рано или поздно приходит время развернуть эрланг-проект на боевом
сервере.  Стандартное решение, которое предлагает OTP фреймворк --
релиз.

В релиз включаются все необходимые приложения. Это и приложения самого
проекта, и зависимые библиотеки, и системные приложения, которые
являются частью OTP.  Каждое приложение представлено папками ebin и
priv. В ebin находятся скомпилированные модули и файл ресурсов, в priv
все остальное, что может понадобится.  Папки include, src, test и
прочие в релиз не включаются.

Далее, в релиз входят скрипты для запуска ноды и загрузки приложений в
нужном порядке. Эти скрипты генерируются автоматически.

По умолчанию в релиз включается и виртуальная машина эрланг. Но ее
можно не включать, если эрланг уже установлен на боевом сервере.

Ну и, конечно, в релиз включаются конфигурационные файлы.

Существуют инструменты, которые автоматически собирают релиз. Это
**systools** и **reltool**, входящие в OTP фреймворк.  И более
современный и удобный [relx](https://github.com/erlware/relx).

Релизы подразумевают, что вашему проекту, кроме эрланг, больше ничего
не нужно.  Часто это не так. Проекту может быть нужна база данных, или
какой-нибудь внешний сервис. Или в нем есть код на других языках
программирования.  Тогда при развертывании на боевом сервере нужно
устанавливать и настраивать базу данных, другие языки
программирования, библиотеки на этих языках, и т.д.

Есть много вариантов, как это можно сделать. Тут и **rpm**/**deb** пакеты,
и инструменты типа [Fabric](http://www.fabfile.org/),
и [Docker](https://www.docker.com/) контейнеры.

Каждая команда делает это по-своему, от конкретных рекомендаций я воздержусь.