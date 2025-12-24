# NSY1110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY1110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭ仕入先統合　　　　　　　　  *
*    サブシステム　　　　：　センター納入業務　　　　　　　　  *
*    モジュール名　　　　：　新ＤＣＭ基本情報ＣＳＶ出力　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/02/22 INOUE                  *
*    処理概要　　　　　　：　条件に従い、基本情報ＣＳＶデータ　*
*                            を作成する。　　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NSY1110B.
*                  流用:SSY1227B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/02/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 基本情報データ　 >>--*
     SELECT   DNJOHOF   ASSIGN    TO        DA-01-VI-DNJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*                       ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-K01
                                            JOH-K02
                                            JOH-K03
                                            JOH-K04
                                            JOH-K05
                                            JOH-K06
                                            JOH-K07
                                            JOH-K08
                        FILE  STATUS   IS   JOH-STATUS.
*----<< ＣＳＶデータ >>----*
     SELECT   CVDNJOHO  ASSIGN    TO        DA-01-S-CVDNJOHO
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報データ　
******************************************************************
 FD  DNJOHOF            LABEL     RECORD   IS   STANDARD.
     COPY     DNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    ＣＳＶデータ
******************************************************************
 FD  CVDNJOHO           LABEL     RECORD   IS   STANDARD.
     COPY     CVDNJOHO  OF        XFDLIB
              JOINING   CSV  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶヘッダ
 01  CSV-HED.
     03  CSV-01S    PIC  X(01)     VALUE  X"28".
     03  CSV-01N    PIC  N(05)     VALUE  NC"バッチ日付".
     03  CSV-01E    PIC  X(01)     VALUE  X"29".
     03  CSV-01K    PIC  X(01)     VALUE  ",".
*
     03  CSV-02S    PIC  X(01)     VALUE  X"28".
     03  CSV-02N    PIC  N(05)     VALUE  NC"バッチ時刻".
     03  CSV-02E    PIC  X(01)     VALUE  X"29".
     03  CSV-02K    PIC  X(01)     VALUE  ",".
*
     03  CSV-03S    PIC  X(01)     VALUE  X"28".
     03  CSV-03N    PIC  N(06)     VALUE  NC"バッチ取引先".
     03  CSV-03E    PIC  X(01)     VALUE  X"29".
     03  CSV-03K    PIC  X(01)     VALUE  ",".
*
     03  CSV-04S    PIC  X(01)     VALUE  X"28".
     03  CSV-04N    PIC  N(04)     VALUE  NC"倉庫ＣＤ".
     03  CSV-04E    PIC  X(01)     VALUE  X"29".
     03  CSV-04K    PIC  X(01)     VALUE  ",".
*
     03  CSV-05S    PIC  X(01)     VALUE  X"28".
     03  CSV-05N    PIC  N(04)     VALUE  NC"店舗ＣＤ".
     03  CSV-05E    PIC  X(01)     VALUE  X"29".
     03  CSV-05K    PIC  X(01)     VALUE  ",".
*
     03  CSV-06S    PIC  X(01)     VALUE  X"28".
     03  CSV-06N    PIC  N(04)     VALUE  NC"伝票番号".
     03  CSV-06E    PIC  X(01)     VALUE  X"29".
     03  CSV-06K    PIC  X(01)     VALUE  ",".
*
     03  CSV-07S    PIC  X(01)     VALUE  X"28".
     03  CSV-07N    PIC  N(03)     VALUE  NC"行番号".
     03  CSV-07E    PIC  X(01)     VALUE  X"29".
     03  CSV-07K    PIC  X(01)     VALUE  ",".
*
     03  CSV-08S    PIC  X(01)     VALUE  X"28".
     03  CSV-08N    PIC  N(03)     VALUE  NC"発注日".
     03  CSV-08E    PIC  X(01)     VALUE  X"29".
     03  CSV-08K    PIC  X(01)     VALUE  ",".
*
     03  CSV-09S    PIC  X(01)     VALUE  X"28".
     03  CSV-09N    PIC  N(05)     VALUE  NC"納品指定日".
     03  CSV-09E    PIC  X(01)     VALUE  X"29".
     03  CSV-09K    PIC  X(01)     VALUE  ",".
*
     03  CSV-10S    PIC  X(01)     VALUE  X"28".
     03  CSV-10N    PIC  N(05)     VALUE  NC"特売コード".
     03  CSV-10E    PIC  X(01)     VALUE  X"29".
     03  CSV-10K    PIC  X(01)     VALUE  ",".
*
     03  CSV-11S    PIC  X(01)     VALUE  X"28".
     03  CSV-11N    PIC  N(04)     VALUE  NC"伝票種別".
     03  CSV-11E    PIC  X(01)     VALUE  X"29".
     03  CSV-11K    PIC  X(01)     VALUE  ",".
*
     03  CSV-12S    PIC  X(01)     VALUE  X"28".
     03  CSV-12N    PIC  N(04)     VALUE  NC"伝票区分".
     03  CSV-12E    PIC  X(01)     VALUE  X"29".
     03  CSV-12K    PIC  X(01)     VALUE  ",".
*
     03  CSV-13S    PIC  X(01)     VALUE  X"28".
     03  CSV-13N    PIC  N(06)     VALUE  NC"発注種別区分".
     03  CSV-13E    PIC  X(01)     VALUE  X"29".
     03  CSV-13K    PIC  X(01)     VALUE  ",".
*
     03  CSV-14S    PIC  X(01)     VALUE  X"28".
     03  CSV-14N    PIC  N(04)     VALUE  NC"納品区分".
     03  CSV-14E    PIC  X(01)     VALUE  X"29".
     03  CSV-14K    PIC  X(01)     VALUE  ",".
*
     03  CSV-15S    PIC  X(01)     VALUE  X"28".
     03  CSV-15N    PIC  N(04)     VALUE  NC"宅配区分".
     03  CSV-15E    PIC  X(01)     VALUE  X"29".
     03  CSV-15K    PIC  X(01)     VALUE  ",".
*
     03  CSV-16S    PIC  X(01)     VALUE  X"28".
     03  CSV-16N    PIC  N(05)     VALUE  NC"拠点コード".
     03  CSV-16E    PIC  X(01)     VALUE  X"29".
     03  CSV-16K    PIC  X(01)     VALUE  ",".
*
     03  CSV-17S    PIC  X(01)     VALUE  X"28".
     03  CSV-17N    PIC  N(04)     VALUE  NC"部門ＣＤ".
     03  CSV-17E    PIC  X(01)     VALUE  X"29".
     03  CSV-17K    PIC  X(01)     VALUE  ",".
*
     03  CSV-18S    PIC  X(01)     VALUE  X"28".
     03  CSV-18N    PIC  N(03)     VALUE  NC"部門名".
     03  CSV-18E    PIC  X(01)     VALUE  X"29".
     03  CSV-18K    PIC  X(01)     VALUE  ",".
*
     03  CSV-19S    PIC  X(01)     VALUE  X"28".
     03  CSV-19N    PIC  N(08)     VALUE  NC"発注事業部コード".
     03  CSV-19E    PIC  X(01)     VALUE  X"29".
     03  CSV-19K    PIC  X(01)     VALUE  ",".
*
     03  CSV-20S    PIC  X(01)     VALUE  X"28".
     03  CSV-20N    PIC  N(06)     VALUE  NC"発注事業部名".
     03  CSV-20E    PIC  X(01)     VALUE  X"29".
     03  CSV-20K    PIC  X(01)     VALUE  ",".
*
     03  CSV-21S    PIC  X(01)     VALUE  X"28".
     03  CSV-21N    PIC  N(06)     VALUE  NC"納品先コード".
     03  CSV-21E    PIC  X(01)     VALUE  X"29".
     03  CSV-21K    PIC  X(01)     VALUE  ",".
*
     03  CSV-22S    PIC  X(01)     VALUE  X"28".
     03  CSV-22N    PIC  N(05)     VALUE  NC"納品先名称".
     03  CSV-22E    PIC  X(01)     VALUE  X"29".
     03  CSV-22K    PIC  X(01)     VALUE  ",".
*
     03  CSV-23S    PIC  X(01)     VALUE  X"28".
     03  CSV-23N    PIC  N(12)
                    VALUE NC"納品センターブロックＣＤ".
     03  CSV-23E    PIC  X(01)     VALUE  X"29".
     03  CSV-23K    PIC  X(01)     VALUE  ",".
*
     03  CSV-24S    PIC  X(01)     VALUE  X"28".
     03  CSV-24N    PIC  N(08)     VALUE  NC"納品センターＣＤ".
     03  CSV-24E    PIC  X(01)     VALUE  X"29".
     03  CSV-24K    PIC  X(01)     VALUE  ",".
*
     03  CSV-25S    PIC  X(01)     VALUE  X"28".
     03  CSV-25N    PIC  N(08)     VALUE  NC"納品センター名称".
     03  CSV-25E    PIC  X(01)     VALUE  X"29".
     03  CSV-25K    PIC  X(01)     VALUE  ",".
*
     03  CSV-26S    PIC  X(01)     VALUE  X"28".
     03  CSV-26N    PIC  N(03)     VALUE  NC"館番号".
     03  CSV-26E    PIC  X(01)     VALUE  X"29".
     03  CSV-26K    PIC  X(01)     VALUE  ",".
*
     03  CSV-27S    PIC  X(01)     VALUE  X"28".
     03  CSV-27N    PIC  N(05)     VALUE  NC"取引先ＣＤ".
     03  CSV-27E    PIC  X(01)     VALUE  X"29".
     03  CSV-27K    PIC  X(01)     VALUE  ",".
*
     03  CSV-28S    PIC  X(01)     VALUE  X"28".
     03  CSV-28N    PIC  N(05)     VALUE  NC"取引先名称".
     03  CSV-28E    PIC  X(01)     VALUE  X"29".
     03  CSV-28K    PIC  X(01)     VALUE  ",".
*
     03  CSV-29S    PIC  X(01)     VALUE  X"28".
     03  CSV-29N    PIC  N(08)     VALUE  NC"個別取引先コード".
     03  CSV-29E    PIC  X(01)     VALUE  X"29".
     03  CSV-29K    PIC  X(01)     VALUE  ",".
*
     03  CSV-30S    PIC  X(01)     VALUE  X"28".
     03  CSV-30N    PIC  N(05)     VALUE  NC"直送コード".
     03  CSV-30E    PIC  X(01)     VALUE  X"29".
     03  CSV-30K    PIC  X(01)     VALUE  ",".
*
     03  CSV-31S    PIC  X(01)     VALUE  X"28".
     03  CSV-31N    PIC  N(02)     VALUE  NC"税率".
     03  CSV-31E    PIC  X(01)     VALUE  X"29".
     03  CSV-31K    PIC  X(01)     VALUE  ",".
*
     03  CSV-32S    PIC  X(01)     VALUE  X"28".
     03  CSV-32N    PIC  N(05)     VALUE  NC"ＪＡＮＣＤ".
     03  CSV-32E    PIC  X(01)     VALUE  X"29".
     03  CSV-32K    PIC  X(01)     VALUE  ",".
*
     03  CSV-33S    PIC  X(01)     VALUE  X"28".
     03  CSV-33N    PIC  N(04)     VALUE  NC"発注行数".
     03  CSV-33E    PIC  X(01)     VALUE  X"29".
     03  CSV-33K    PIC  X(01)     VALUE  ",".
*
     03  CSV-34S    PIC  X(01)     VALUE  X"28".
     03  CSV-34N    PIC  N(03)     VALUE  NC"商品名".
     03  CSV-34E    PIC  X(01)     VALUE  X"29".
     03  CSV-34K    PIC  X(01)     VALUE  ",".
*
     03  CSV-35S    PIC  X(01)     VALUE  X"28".
     03  CSV-35N    PIC  N(06)     VALUE  NC"商品規格名称".
     03  CSV-35E    PIC  X(01)     VALUE  X"29".
     03  CSV-35K    PIC  X(01)     VALUE  ",".
*
     03  CSV-36S    PIC  X(01)     VALUE  X"28".
     03  CSV-36N    PIC  N(06)     VALUE  NC"ゴンドラ番号".
     03  CSV-36E    PIC  X(01)     VALUE  X"29".
     03  CSV-36K    PIC  X(01)     VALUE  ",".
*
     03  CSV-37S    PIC  X(01)     VALUE  X"28".
     03  CSV-37N    PIC  N(06)     VALUE  NC"発注単位区分".
     03  CSV-37E    PIC  X(01)     VALUE  X"29".
     03  CSV-37K    PIC  X(01)     VALUE  ",".
*
     03  CSV-38S    PIC  X(01)     VALUE  X"28".
     03  CSV-38N    PIC  N(04)     VALUE  NC"形状区分".
     03  CSV-38E    PIC  X(01)     VALUE  X"29".
     03  CSV-38K    PIC  X(01)     VALUE  ",".
*
     03  CSV-39S    PIC  X(01)     VALUE  X"28".
     03  CSV-39N    PIC  N(04)     VALUE  NC"発注数量".
     03  CSV-39E    PIC  X(01)     VALUE  X"29".
     03  CSV-39K    PIC  X(01)     VALUE  ",".
*
     03  CSV-40S    PIC  X(01)     VALUE  X"28".
     03  CSV-40N    PIC  N(03)     VALUE  NC"原単価".
     03  CSV-40E    PIC  X(01)     VALUE  X"29".
     03  CSV-40K    PIC  X(01)     VALUE  ",".
*
     03  CSV-41S    PIC  X(01)     VALUE  X"28".
     03  CSV-41N    PIC  N(03)     VALUE  NC"売単価".
     03  CSV-41E    PIC  X(01)     VALUE  X"29".
     03  CSV-41K    PIC  X(01)     VALUE  ",".
*
     03  CSV-42S    PIC  X(01)     VALUE  X"28".
     03  CSV-42N    PIC  N(06)     VALUE  NC"発注原価金額".
     03  CSV-42E    PIC  X(01)     VALUE  X"29".
     03  CSV-42K    PIC  X(01)     VALUE  ",".
*
     03  CSV-43S    PIC  X(01)     VALUE  X"28".
     03  CSV-43N    PIC  N(06)     VALUE  NC"発注売価金額".
     03  CSV-43E    PIC  X(01)     VALUE  X"29".
     03  CSV-43K    PIC  X(01)     VALUE  ",".
*
     03  CSV-44S    PIC  X(01)     VALUE  X"28".
     03  CSV-44N    PIC  N(06)     VALUE  NC"発注伝票番号".
     03  CSV-44E    PIC  X(01)     VALUE  X"29".
*
*
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  DNJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  IX                      PIC  9(05)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
*ブレイク項目
 01  BRK-F001              PIC  9(08)   VALUE ZERO.
 01  BRK-F002              PIC  9(04)   VALUE ZERO.
 01  BRK-F003              PIC  9(08)   VALUE ZERO.
 01  BRK-F004              PIC  X(02)   VALUE SPACE.
 01  BRK-FA01              PIC  9(04)   VALUE ZERO.
 01  BRK-FA02              PIC  9(06)   VALUE ZERO.
 01  BRK-FA03              PIC  9(02)   VALUE ZERO.
 01  BRK-FA06              PIC  9(02)   VALUE ZERO.
 01  BRK-FA04              PIC  9(06)   VALUE ZERO.
 01  BRK-FA05              PIC  9(06)   VALUE ZERO.
*
 01  WK-ST.
     03  JOH-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CSV-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NSY1110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY1110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY1110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
*01  PARA-IN-BUMON             PIC   X(04).
*01  PARA-IN-TANTOU            PIC   X(02).
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-STEN              PIC   9(05).
*01  PARA-IN-ETEN              PIC   9(05).
*01  PARA-IN-SROUTE            PIC   9(02).
*01  PARA-IN-EROUTE            PIC   9(02).
*01  PARA-IN-SBUMON            PIC   9(02).
*01  PARA-IN-EBUMON            PIC   9(02).
*01  PARA-IN-HDATE             PIC   9(08).
*01  PARA-IN-NDATE             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
*                                 PARA-IN-BUMON
*                                 PARA-IN-TANTOU
                                  PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO.
*                                 PARA-IN-STEN
*                                 PARA-IN-ETEN
*                                 PARA-IN-SROUTE
*                                 PARA-IN-EROUTE
*                                 PARA-IN-SBUMON
*                                 PARA-IN-EBUMON
*                                 PARA-IN-HDATE
*                                 PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNJOHOF.
     MOVE      "DNJOHOL1 "   TO   AB-FILE.
     MOVE      JOH-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVDNJOHO.
     MOVE      "CVDNJOHO"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DNJOHOF.
     OPEN     OUTPUT    CVDNJOHO.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG
                                             TENMS1-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    基本情報データ　　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE  UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME  UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*T
     MOVE      SPACE           TO   JOH-REC.
     INITIALIZE                     JOH-REC.
     MOVE      PARA-IN-JDATE   TO   JOH-K01.
     MOVE      PARA-IN-JTIME   TO   JOH-K02.
     MOVE      PARA-IN-TORICD  TO   JOH-K03.
     MOVE      PARA-IN-SOKO    TO   JOH-K04.
     START     DNJOHOF   KEY   >=   JOH-K01  JOH-K02
                                    JOH-K03  JOH-K04
                                    JOH-K05  JOH-K06
                                    JOH-K07  JOH-K08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    基本情報データ　読込み
     PERFORM DNJOHOF-READ-SEC.
*
*    ＣＳＶヘッダ出力
     MOVE    SPACE       TO       CSV-REC.
     WRITE   CSV-REC     FROM     CSV-HED.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　基本情報データ　読込み　　　　　　　　　　*
****************************************************************
 DNJOHOF-READ-SEC    SECTION.
*
     MOVE    "DNJOHOF-READ-SEC"    TO  S-NAME.
*
     READ     DNJOHOF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DNJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 DNJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
*
 MAIN001.
     IF ( PARA-IN-JDATE     =    JOH-K01 ) AND
        ( PARA-IN-JTIME     =    JOH-K02 ) AND
        ( PARA-IN-TORICD    =    JOH-K03 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN002.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     JOH-K04     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN888.
*
     PERFORM CVDNJOHO-WRITE-SEC.
*    MOVE    JOH-K01                TO  BRK-F001.
*    MOVE    JOH-K02                TO  BRK-F002.
*    MOVE    JOH-K03                TO  BRK-F003.
*    MOVE    JOH-K04                TO  BRK-F004.
*
 MAIN999.
*    基本情報データ　読込み
     PERFORM DNJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ＣＳＶデータ出力処理
****************************************************************
 CVDNJOHO-WRITE-SEC  SECTION.
*
     MOVE     "CVDNJOHO-WRITE-SEC" TO      S-NAME.
*
     MOVE      SPACE               TO      CSV-REC.
     INITIALIZE                            CSV-REC.
     MOVE      ","                 TO      CSV-N01
                                           CSV-N02
                                           CSV-N03
                                           CSV-N04
                                           CSV-N05
                                           CSV-N06
                                           CSV-N07
                                           CSV-N08
                                           CSV-N09
                                           CSV-N10
                                           CSV-N11
                                           CSV-N12
                                           CSV-N13
                                           CSV-N14
                                           CSV-N15
                                           CSV-N16
                                           CSV-N17
                                           CSV-N18
                                           CSV-N19
                                           CSV-N20
                                           CSV-N21
                                           CSV-N22
                                           CSV-N23
                                           CSV-N24
                                           CSV-N25
                                           CSV-N26
                                           CSV-N27
                                           CSV-N28
                                           CSV-N29
                                           CSV-N30
                                           CSV-N31
                                           CSV-N32
                                           CSV-N33
                                           CSV-N34
                                           CSV-N35
                                           CSV-N36
                                           CSV-N37
                                           CSV-N38
                                           CSV-N39
                                           CSV-N40
                                           CSV-N41
                                           CSV-N42
                                           CSV-N43.
*                                          CSV-N44.
     MOVE      X"28"               TO      CSV-F181
                                           CSV-F201
                                           CSV-F221
                                           CSV-F251
                                           CSV-F281
                                           CSV-F341
                                           CSV-F351.
     MOVE      X"29"               TO      CSV-F182
                                           CSV-F202
                                           CSV-F222
                                           CSV-F252
                                           CSV-F282
                                           CSV-F342
                                           CSV-F352.
     MOVE      JOH-K01             TO      CSV-F01.
     MOVE      JOH-K02             TO      CSV-F02.
     MOVE      JOH-K03             TO      CSV-F03.
     MOVE      JOH-K04             TO      CSV-F04.
     MOVE      JOH-K05             TO      CSV-F05.
     MOVE      JOH-K06             TO      CSV-F06.
     MOVE      JOH-K07             TO      CSV-F07.
     MOVE      JOH-F05             TO      CSV-F08.
     MOVE      JOH-F06             TO      CSV-F09.
     MOVE      JOH-F061            TO      CSV-F10.
     MOVE      JOH-F08             TO      CSV-F11.
     MOVE      JOH-F081            TO      CSV-F12.
     MOVE      JOH-F09             TO      CSV-F13.
     MOVE      JOH-F10             TO      CSV-F14.
     MOVE      JOH-F101            TO      CSV-F15.
     MOVE      JOH-F134            TO      CSV-F16.
     MOVE      JOH-F14             TO      CSV-F17.
     MOVE      JOH-F162            TO      CSV-F18.
     MOVE      JOH-F17             TO      CSV-F19.
     MOVE      JOH-F192            TO      CSV-F20.
     MOVE      JOH-F21             TO      CSV-F21.
     MOVE      JOH-F232            TO      CSV-F22.
     MOVE      JOH-F234            TO      CSV-F23.
     MOVE      JOH-F24             TO      CSV-F24.
     MOVE      JOH-F262            TO      CSV-F25.
     MOVE      JOH-F264            TO      CSV-F26.
     MOVE      JOH-F27             TO      CSV-F27.
     MOVE      JOH-F302            TO      CSV-F28.
     MOVE      JOH-F304            TO      CSV-F29.
     MOVE      JOH-F31             TO      CSV-F30.
     MOVE      JOH-F32             TO      CSV-F31.
     MOVE      JOH-M02             TO      CSV-F32.
     MOVE      JOH-M03             TO      CSV-F33.
     MOVE      JOH-M082            TO      CSV-F34.
     MOVE      JOH-M0852           TO      CSV-F35.
     MOVE      JOH-M086            TO      CSV-F36.
     MOVE      JOH-M09             TO      CSV-F37.
     MOVE      JOH-M091            TO      CSV-F38.
     MOVE      JOH-M11             TO      CSV-F39.
     MOVE      JOH-M16             TO      CSV-F40.
     MOVE      JOH-M17             TO      CSV-F41.
     MOVE      JOH-M14             TO      CSV-F42.
     MOVE      JOH-M15             TO      CSV-F43.
     MOVE      JOH-M19             TO      CSV-F44.
*
     WRITE     CSV-REC.
     ADD       1                   TO      WRT-CNT.
*
 CVDNJOHO-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"基本情報"   "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"ＣＳＶ　"   "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY  NC"　　　　　　"   "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     DNJOHOF  CVDNJOHO.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
