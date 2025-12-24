****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　*
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　ＡＭＡＺＯＮ受注データ変換　　　　*
*    作成日／作成者　　　：　2020/11/09 INOUE                  *
*    処理概要　　　　　　：　受注ファイルより、
*                            伝票更新処理（現行）に引き渡す。
*                            また、出荷予定・実績管理用に、
*                            基本情報ファイルを作成する。
*    更新日／更新者　　　：　2020/12/23 NAV TAKAHASHI　　　　
*    更新内容　　　　　　：  伝票単位を６行から１行へ変更
*    　　　　　　　　　　：  付番ＦＬＧセット変更　　　　　
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            NJH9310B.
*                 流用：NJH3754B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/11/09.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*アマゾン受注ファイル
     SELECT   AMZJYUL1  ASSIGN    TO        DA-01-VI-AMZJYUL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYU-FA04
                                            JYU-FA03
                                            JYU-FA09
                        FILE      STATUS    JYU-STATUS.
*アマゾン受注ファイル（件数・枚数カウント用）
     SELECT   AMZJYUL2  ASSIGN    TO        DA-01-VI-AMZJYUL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JY2-FE01
                                            JY2-FE02
                                            JY2-FE03
                                            JY2-FB09
                                            JY2-FB01
                                            JY2-FB02
                        FILE      STATUS    JY2-STATUS.
*変換伝票データ(当日売上伝票ファイル)
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*アマゾン基本情報ファイル
     SELECT   AMZJOHL1  ASSIGN    TO        DA-01-VI-AMZJOHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JOH-FA04
                                            JOH-FA03
                                            JOH-FA09
                        FILE      STATUS    JOH-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*受信件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    アマゾン受注ファイル
******************************************************************
 FD  AMZJYUL1            LABEL RECORD   IS   STANDARD.
     COPY     AMZJYUL1   OF        XFDLIB
              JOINING    JYU       PREFIX.
******************************************************************
*    アマゾン受注ファイル
******************************************************************
 FD  AMZJYUL2            LABEL RECORD   IS   STANDARD.
     COPY     AMZJYUL2   OF        XFDLIB
              JOINING    JY2       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
******************************************************************
*    アマゾン基本情報ファイル
******************************************************************
 FD  AMZJOHL1
                        LABEL RECORD   IS   STANDARD.
     COPY     AMZJOHL1  OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    受信件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1            LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1   OF        XFDLIB
              JOINING  MEI       PREFIX.
*
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU-D             PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  WK-SAIBAN               PIC  9(11)     VALUE  ZERO.
 01  SAKUBAF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(05)V9(01)  VALUE  ZERO.
 01  WK-JOH-F21-1            PIC  9(05)     VALUE  ZERO.
 01  FURISOK                 PIC  X(02)     VALUE  SPACE.
 01  JY2-END                 PIC  X(03)     VALUE  SPACE.
*
*伝票ブレイク（ＡＭＡＺＯＮ発注書番号系）
 01  BRK-FA04                PIC  X(08)     VALUE  ZERO.
 01  BRK-FA03                PIC  X(10)     VALUE  SPACE.
 01  BRK-FA09                PIC  9(03)     VALUE  ZERO.
*
*伝票ブレイク（件数・枚数カウント用）
 01  BRK-FE01                PIC  9(08)     VALUE  ZERO.
 01  BRK-FE02                PIC  9(04)     VALUE  ZERO.
 01  BRK-FE03                PIC  9(08)     VALUE  ZERO.
 01  BRK-FB09                PIC  X(02)     VALUE  SPACE.
 01  BRK-FB01                PIC  X(09)     VALUE  ZERO.
*
*基幹伝票行カウント
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
*
*伝票番号取得ＳＵＢ用パラメタ
 01  LINK-AREA-SUB1.
     03  LI-TRCD             PIC  9(08).
     03  LO-ERR              PIC  9(01).
     03  LO-DENNO            PIC  9(09).
     03  LO-NEXT             PIC  9(09).
 01  WRK-DEN.
     03  OUT-DENNO           PIC  9(09)     VALUE  ZERO.
*
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*伝票NO編集
 01  HEN-DEN.
     03  HEN-JYU-TEN         PIC  9(02)     VALUE  ZERO.
     03  HEN-JYU-DEN         PIC  9(07)     VALUE  ZERO.
*
 01  WK-HEN-A24.
     03  WK-JYU-A83          PIC  9(03).
     03  FILLER              PIC  X(01)     VALUE  SPACE.
     03  WK-JYU-A87          PIC  X(15).
*
 01  WK-HEN-A28.
     03  FILLER              PIC  X(07)     VALUE  "ｺｳｺｸNO:".
     03  WK-JYU-A81          PIC  9(08).
*
 01  WK-JYU-A56              PIC  9(05)V9(01).
 01  WK-JYU-A56R             REDEFINES      WK-JYU-A56.
     03  WK-HEN-A41          PIC  9(05).
     03  WK-HEN-A42          PIC  9(01).
*
 01  WK-JYU-A63              PIC  9(07)V9(02).
 01  WK-JYU-A63R             REDEFINES      WK-JYU-A63.
     03  WK-HEN-A43          PIC  9(07).
     03  WK-HEN-A44          PIC  9(02).
*伝票番号
*01  WK-DENNO                PIC  9(07).
*
*伝票ヘッダ退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(02).
     03  WK-DEPB03          PIC  X(02).
     03  WK-DEPB04          PIC  9(07).
     03  WK-DEPB05.
       05  WK-DEPB051       PIC  9(02).
       05  WK-DEPB052       PIC  9(03).
     03  WK-DEPB06          PIC  X(04).
     03  WK-DEPB07          PIC  X(02).
     03  WK-DEPB08          PIC  9(02).
     03  WK-DEPB09          PIC  9(02).
     03  WK-DEPB10          PIC  9(06).
     03  WK-DEPB11          PIC  9(06).
     03  WK-DEPB12          PIC  9(06).
     03  WK-DEPB13          PIC  X(02).
     03  WK-DEPB14          PIC  X(15).
     03  WK-DEPB15          PIC  X(15).
     03  WK-DEPB16          PIC  X(15).
     03  WK-DEPB17          PIC  X(12).
     03  WK-DEPB18          PIC  9(01).
     03  WK-DEPB181         PIC  X(01).
     03  WK-DEPB19          PIC  X(21).
     03  WK-DEPB20          PIC  9(01).
*
*    伝票ヘッダ・オプション退避ワーク
 01  WK-DEPC-REC.
     03  WK-DEPC01          PIC  X(01).
     03  WK-DEPC02          PIC  9(02).
     03  WK-DEPC03          PIC  X(07).
     03  WK-DEPC04          PIC  X(26).
     03  WK-DEPC05          PIC  X(14).
     03  WK-DEPC06          PIC  X(07).
     03  WK-DEPC07          PIC  X(05).
     03  WK-DEPC08          PIC  X(15).
     03  WK-DEPC09          PIC  X(07).
     03  WK-DEPC10          PIC  X(16).
     03  WK-DEPC11          PIC  X(20).
     03  WK-DEPC12          PIC  X(07).
     03  WK-DEPC13          PIC  9(01).
*
*    伝票明細行退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X.
     03  WK-DEPD02          PIC  9(02).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  X(13).
     03  WK-DEPD05          PIC  X(06).
     03  WK-DEPD06          PIC  X(01).
     03  WK-DEPD07          PIC  X(03).
     03  WK-DEPD08          PIC  9(05).
     03  WK-DEPD09          PIC  X(01).
     03  WK-DEPD10          PIC  9(07).
     03  WK-DEPD11          PIC  X(02).
     03  WK-DEPD12          PIC  9(07).
     03  WK-DEPD13          PIC  9(10).
     03  WK-DEPD14          PIC  9(10).
     03  WK-DEPD15          PIC  X(09).
     03  WK-DEPD16          PIC  X(25).
     03  WK-DEPD17          PIC  X(08).
     03  WK-DEPD18          PIC  X(06).
     03  WK-DEPD19          PIC  X(05).
     03  WK-DEPD20          PIC  X(04).
     03  WK-DEPD21          PIC  9(01).
*
*    伝票明細行オプション退避ワーク
 01  WK-DEPE-REC.
     03  WK-DEPE01          PIC  X.
     03  WK-DEPE02          PIC  9(02).
     03  WK-DEPE03          PIC  X(25).
     03  WK-DEPE04          PIC  X.
     03  WK-DEPE05          PIC  X.
     03  WK-DEPE06          PIC  9(02).
     03  WK-DEPE07          PIC  X(13).
     03  WK-DEPE08          PIC  X(20).
     03  WK-DEPE09          PIC  X(20).
     03  WK-DEPE10          PIC  9(07).
     03  WK-DEPE11          PIC  9(05).
     03  WK-DEPE12          PIC  X(30).
     03  WK-DEPE13          PIC  X(01).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  JYU-STATUS        PIC  X(02).
     03  JY2-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
*    03  JYO-STATUS        PIC  X(02).
*    03  SOK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH9310B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9310B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9310B".
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
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
*
 DECLARATIVES.
 FILEERR-SEC0           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJYUL1.
     MOVE      "AMZJYUL1"   TO   AB-FILE.
     MOVE      JYU-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJYUL2.
     MOVE      "AMZJYUL2"   TO   AB-FILE.
     MOVE      JY2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJOHL1.
     MOVE      "AMZJOHL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1 "    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       AMZJYUL1.
     OPEN     INPUT     SHOTBL1   MEIMS1
                        TENMS1    TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1  AMZJOHL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPC-REC.
     INITIALIZE                   WK-DEPC-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPE-REC.
     INITIALIZE                   WK-DEPE-REC.
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
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
 INIT-01.
*AMAZON受注ファイル READ
     READ     AMZJYUL1
              AT END
                 MOVE      9         TO  END-FG
                 GO                  TO  INIT-EXIT
              NOT AT END
                 ADD       1         TO  RD-CNT
     END-READ.
 INIT-02.
*基幹伝票NO＝""  基幹行NO=ZEROか空白　のレコードのみ対象
     IF  ( JYU-FB01     =   SPACE  )        AND
         ( JYU-FB02     =   SPACE OR "00"  )
           MOVE      JYU-FA04        TO  BRK-FA04
           MOVE      JYU-FA03        TO  BRK-FA03
           MOVE      0               TO  CNT-GYO
     ELSE
           GO                        TO  INIT-01
     END-IF.
*
*
*基幹伝票ＮＯ採番
*#2020/12/23 NAV ST １伝票６行→１伝票１行へ変更
*##  PERFORM   DPNO-SET-SEC.
*##  ADD       1           TO   CNT-MAISU.
*#2020/12/23 NAV EDE
*
 INIT-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DPNO-SET-SEC                 SECTION.
     MOVE     "DPNO-SET-SEC"       TO   S-NAME.
*
     INITIALIZE                        LINK-AREA-SUB1.
*
 DPNO-010.
     MOVE      JYU-FE03      TO        LI-TRCD.
     CALL     "SKYSBCK1"     USING     LINK-AREA-SUB1.
 DPNO-020.
*↓TEST
*    DISPLAY   "LO-ERR ="  LO-ERR    UPON CONS.
*    DISPLAY   "LO-DENO="  LO-DENNO  UPON CONS.
*    DISPLAY   "LO-NEXT="  LO-NEXT   UPON CONS.
*↑TEST
     IF       LO-ERR  =  0
              MOVE      LO-NEXT     TO        OUT-DENNO
              MOVE      SPACE       TO        TOKMS2-INV-FLG
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   LI-TRCD               UPON CONS
              MOVE      "INV"       TO        TOKMS2-INV-FLG
              MOVE      4001        TO        PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*
 MAIN-01.
*基幹伝票NO＝""  基幹行NO=ZEROか空白　のレコードのみ対象
     IF  ( JYU-FB01     =   SPACE  )   AND
         ( JYU-FB02     =   SPACE OR "00"  )
           CONTINUE
     ELSE
           GO                    TO    MAIN-99
     END-IF.
*
 MAIN-02.
*キーブレイク　OR  明細6行超　→　基幹伝票ＮＯ採番
*#2020/12/23 NAV ST １伝票６行→１伝票１行へ変更
*##  IF  ( JYU-FA04  NOT =  BRK-FA04  )   OR
*##      ( JYU-FA03  NOT =  BRK-FA03  )   OR
*##      ( CNT-GYO   >      5         )
*##        PERFORM   DPNO-SET-SEC
*##        ADD       1               TO  CNT-MAISU
*##        MOVE      1               TO  CNT-GYO
*##        MOVE      JYU-FA04        TO  BRK-FA04
*##        MOVE      JYU-FA03        TO  BRK-FA03
*##  ELSE
*##        ADD       1               TO  CNT-GYO
*##  END-IF.
     PERFORM   DPNO-SET-SEC.
     ADD       1               TO  CNT-MAISU.
     MOVE      1               TO  CNT-GYO.
     MOVE      JYU-FA04        TO  BRK-FA04.
     MOVE      JYU-FA03        TO  BRK-FA03.
*#2020/12/23 NAV ED １伝票６行→１伝票１行へ変更
*
 MAIN-03.
* 得意先マスタ検索
     MOVE    SPACE         TO    TOK-REC.
     INITIALIZE                  TOK-REC.
     MOVE    JYU-FE03      TO    TOK-F01.
     READ    TOKMS2
             INVALID
                 MOVE  SPACE     TO    TOK-REC
                 INITIALIZE            TOK-REC
                 MOVE  "INV"     TO    TOKMS2-INV-FLG
     END-READ.
*
* 店舗マスタ検索
     MOVE    SPACE         TO    TEN-REC.
     INITIALIZE                  TEN-REC.
     MOVE    JYU-FE03      TO    TEN-F52.
     MOVE    JYU-FB11      TO    TEN-F011.
     READ    TENMS1
             INVALID
                 MOVE  SPACE     TO    TEN-REC
                 INITIALIZE            TEN-REC
                 MOVE  "INV"     TO    TENMS1-INV-FLG
     END-READ.
*
* 商品変換テーブル検索
     MOVE        SPACE     TO    TBL-REC.
     INITIALIZE                  TBL-REC.
     MOVE        JYU-FE03  TO    TBL-F01.
     MOVE        JYU-FA14  TO    TBL-F02.
     READ    SHOTBL1
             INVALID
                 MOVE  SPACE     TO    TBL-REC
                 INITIALIZE            TBL-REC
                 MOVE  "INV"     TO    SHOTBL1-INV-FLG
     END-READ
     MOVE    TBL-F04             TO    WK-RUTO.
*
* 商品名称マスタ検索
     MOVE        SPACE     TO    MEI-REC.
     INITIALIZE                  MEI-REC.
     IF          SHOTBL1-INV-FLG =     SPACE
                 MOVE  TBL-F031  TO    MEI-F011
                 MOVE  TBL-F0321 TO    MEI-F0121
                 MOVE  TBL-F0322 TO    MEI-F0122
                 MOVE  TBL-F0323 TO    MEI-F0123
                 PERFORM   MEIMS1-READ-SEC
                 IF    MEIMS1-INV-FLG  = "INV"
                       MOVE   SPACE    TO    MEI-REC
                       INITIALIZE            MEI-REC
                 END-IF
     END-IF.
*------------------------------------------------
     MOVE    JYU-FE03            TO    WK-TOKCD.
*------------------------------------------------
*    ↓最後にカウントしなおす
*--- ブレーク時、出荷場所件数マスタ出力
*---       IF   WK-DENNO     NOT =     JYU-A23
*---            PERFORM JSMKENL1-WRT-SEC
*---            ADD     1         TO   CNT-MAISU
*---            MOVE    JYU-A23   TO   WK-DENNO
*---            MOVE    ZERO      TO   CNT-KENSU-D
*---       END-IF
*---       ADD       1            TO   CNT-KENSU-D
*---       ADD       1            TO   CNT-KENSU
*---------------------------------------------------
* ファイル編集・出力
     PERFORM  EDIT-SEC.
* アマゾン受注ファイルに基幹伝票ＮＯ・行ＮＯ・振分倉庫ＣＤを更新
     MOVE     OUT-DENNO    TO   JYU-FB01.
     MOVE     CNT-GYO      TO   JYU-FB02.
     MOVE     FURISOK      TO   JYU-FB09.
     REWRITE  JYU-REC.
*
*
 MAIN-99.
     READ     AMZJYUL1
              AT END    MOVE      9    TO  END-FG
              NOT AT END
                        ADD       1    TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC  JOH-REC.
     INITIALIZE                       HEN-REC  JOH-REC.
*
 EDIT-01.
*各量販店領域転送
*  不使用　転送なし
*
 EDIT-02.
*ファイル項目転送
     PERFORM  TENSO-SEC.
*
 EDIT-03.
*当日売上伝票ファイル更新
     WRITE    HEN-REC.
*
 EDIT-04.
*基本情報ファイル更新
     WRITE    JOH-REC.
*
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　　ファイル項目転送                              *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*当日売上伝票ファイルレコード
*取引先コード
     MOVE        JYU-FE03  TO        HEN-F01.
*伝票番号
     MOVE        OUT-DENNO TO        HEN-F02
                                     HEN-F23.
*行番号
     MOVE        CNT-GYO   TO        HEN-F03.
*相殺区分
     MOVE        0         TO        HEN-F04.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        JYU-FB11  TO        HEN-F07.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*注文番号
     MOVE        SPACE     TO        HEN-F10.
*振分ルート取得
*    MOVE       TBL-F04    TO        WK-RUTO.
*    MOVE       HEN-F02    TO        WK-DENNO.
*---------------------------*
*発注日
     MOVE        JYU-FA04  TO        HEN-F111.
*納品日
     MOVE        JYU-FA04  TO        HEN-F112.
*出荷日
     MOVE        ZERO      TO        HEN-F113.
*分類コード
     MOVE        SPACE     TO        HEN-F12.
*商品区分
     MOVE        SPACE     TO        HEN-F131.
*伝票区分
     MOVE        SPACE     TO        HEN-F132.
*請求
     MOVE        ZERO      TO        HEN-F133.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411.
*自社品単コード
     MOVE        TBL-F032  TO        HEN-F1412.
*商品名
     MOVE        MEI-F031  TO        HEN-F1421.
     MOVE        MEI-F032  TO        HEN-F1422.
*数量
     MOVE        JYU-FA10  TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*仕入単価
     MOVE        ZERO      TO        HEN-F171.
*原価単価
     MOVE        JYU-FA11  TO        HEN-F172.
*売価単価
     MOVE        ZERO      TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181   =        HEN-F15   *  HEN-F172.
*売価金額
     COMPUTE     HEN-F182   =        HEN-F15   *  HEN-F173.
*消費税
     MOVE        ZERO      TO        HEN-F19.
*粗利
     MOVE        ZERO      TO        HEN-F20.
*ストック番号
     MOVE        SPACE     TO        HEN-F21.
*備考  発注書番号をセットしておく
     MOVE        JYU-FA03  TO        HEN-F22.
*自社得意先コード
     MOVE        TOK-F52   TO        HEN-F24.
*相手商品コード
     MOVE        JYU-FA14  TO        HEN-F25.
*請求フラグ
     MOVE        ZERO      TO        HEN-F261.
*フラグ2
     MOVE        ZERO      TO        HEN-F262.
*フラグ3
     MOVE        ZERO      TO        HEN-F263.
*フラグ4
     MOVE        ZERO      TO        HEN-F264.
*在庫計上フラグ
     MOVE        ZERO      TO        HEN-F265.
*チェックリスト
     MOVE        ZERO      TO        HEN-F271.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*修正
     MOVE        ZERO      TO        HEN-F273.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*付番区分
*#2020/12/23 NAV ST
*****MOVE        9         TO        HEN-F276.
     MOVE        0         TO        HEN-F276.
*#2020/12/23 NAV ED
*売上データ作成
     MOVE        ZERO      TO        HEN-F277.
*量販店区分
     MOVE       "A"        TO        HEN-F278.
*端数金額
     MOVE        ZERO      TO        HEN-F279.
*端数消費税
     MOVE        ZERO      TO        HEN-F27A.
*付番修正
     MOVE        ZERO      TO        HEN-F27B.
*在庫引落フラグ
     MOVE        ZERO      TO        HEN-F27C.
*在庫引当フラグ
     MOVE        ZERO      TO        HEN-F27D.
*予備4
     MOVE        ZERO      TO        HEN-F27E.
*予備5
     MOVE        ZERO      TO        HEN-F27F.
*ＷＳ番号
     MOVE        1         TO        HEN-F28.
*変換値
     MOVE        ZERO      TO        HEN-F29.
*店舗名（カナ）
     MOVE        TEN-F04   TO        HEN-F30.
*伝票枚数
     MOVE        ZERO      TO        HEN-F98.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*オンライン伝票枚数
     MOVE        ZERO      TO        HEN-F40.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
*    出荷場所特定区分（1:ルート条件マスタ は不使用）
     IF       TOK-F95      =  2
*       明細行番号
        IF       CNT-GYO      =  1
*          出荷場所
           IF      TBL-F04   NOT =  SPACE
*T
*                  DISPLAY "TBL-F02=" TBL-F02 UPON CONS
*                  DISPLAY "TBL-F04=" TBL-F04 UPON CONS
*T
                   MOVE    TBL-F04   TO       FURISOK
           ELSE
                   MOVE    TOK-F81   TO       FURISOK
           END-IF
        ELSE
           CONTINUE
        END-IF
     ELSE
*          代表倉庫
           MOVE  TOK-F81             TO       FURISOK
     END-IF.
     MOVE        FURISOK   TO        HEN-F48.
*棚番
     MOVE        JYU-FB10  TO        HEN-F49.
*訂正前数量
     MOVE        JYU-FA10  TO        HEN-F50.
*訂正仕入単価
     MOVE        ZERO      TO        HEN-F511.
*修正原価単価
     MOVE        JYU-FA11  TO        HEN-F512.
*修正売価単価
     MOVE        ZERO      TO        HEN-F513.
*修正原価金額
     COMPUTE     HEN-F521   =        HEN-F50   *  HEN-F512.
*修正売価金額
     COMPUTE     HEN-F522   =        HEN-F50   *  HEN-F513.
*訂正フラグ
     MOVE        ZERO      TO        HEN-F53.
*更新済フラグ
     MOVE        ZERO      TO        HEN-F54.
*エラーフラグ
     MOVE        ZERO      TO        HEN-F55.
*----------------------------------------------
*----------------------------------------------
*基本情報ファイルレコード
*共通情報フィールド
     MOVE    JYU-F0        TO        JOH-F0.
*受注情報フィールド
     MOVE    JYU-FA        TO        JOH-FA.
*引当フィールド
     MOVE    JYU-FB        TO        JOH-FB.
*  基幹伝票ＮＯ
     MOVE    OUT-DENNO     TO        JOH-FB01.
*  基幹行ＮＯ
     MOVE    CNT-GYO       TO        JOH-FB02.
*  倉庫コード
     MOVE    FURISOK       TO        JOH-FB09.
*検品実績フィールド
     MOVE    JYU-FC        TO        JOH-FC.
*予備フィールド
     MOVE    JYU-FD        TO        JOH-FD.
*付加情報フィールド
     MOVE    JYU-FE        TO        JOH-FE.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
*
 JSMKENL1-WRT-00.
*初期ＲＥＡＤ
     READ    AMZJYUL2
             AT  END
                 GO               TO   JSMKENL1-WRT-EXIT
             NOT AT  END
                 MOVE   JY2-FE01  TO   BRK-FE01
                 MOVE   JY2-FE02  TO   BRK-FE02
                 MOVE   JY2-FE03  TO   BRK-FE03
                 MOVE   JY2-FB09  TO   BRK-FB09
                 MOVE   JY2-FB01  TO   BRK-FB01
     END-READ.
*
 JSMKENL1-WRT-01.
     MOVE    1                  TO   CNT-MAISU CNT-MAISU-D.
     MOVE    1                  TO   CNT-KENSU.
     MOVE    ZERO               TO   CNT-KENSU-D.
     GO                         TO   JSMKENL1-WRT-03.
*
 JSMKENL1-WRT-02.
*順ＲＥＡＤ
     READ    AMZJYUL2
             AT  END
                 MOVE  "END"    TO   JY2-END
                 GO             TO   JSMKENL1-WRT-04
             NOT AT END
                 ADD    1       TO   CNT-KENSU
     END-READ.
*
 JSMKENL1-WRT-03.
*当日スケジュールマスタ用
     IF      JY2-FB01       NOT =      BRK-FB01
             ADD    1       TO         CNT-MAISU
     END-IF.
*T
*    DISPLAY "TOTAL       "           UPON CONS.
*    DISPLAY "  CNT-KENSU=" CNT-KENSU UPON CONS.
*    DISPLAY "  JY2-FB01 =" JY2-FB01  UPON CONS.
*    DISPLAY "  BRL-FB01 =" BRK-FB01  UPON CONS.
*    DISPLAY "  CNT-MAISU=" CNT-MAISU UPON CONS.
*T
*
*件数ファイル用
*T
*    DISPLAY "BETSU       "           UPON CONS.
*    DISPLAY "  JY2-FE01 =" JY2-FE01  UPON CONS.
*    DISPLAY "  BRK-FE01 =" BRK-FE01  UPON CONS.
*    DISPLAY "  JY2-FE02 =" JY2-FE02  UPON CONS.
*    DISPLAY "  BRK-FE02 =" BRK-FE02  UPON CONS.
*    DISPLAY "  JY2-FE03 =" JY2-FE03  UPON CONS.
*    DISPLAY "  BRK-FE03 =" BRK-FE03  UPON CONS.
*    DISPLAY "  JY2-FB09 =" JY2-FB09  UPON CONS.
*    DISPLAY "  BRK-FB09 =" BRK-FB09  UPON CONS.
*    DISPLAY "  JY2-FB01 =" JY2-FB01  UPON CONS.
*    DISPLAY "  BRK-FB01 =" BRK-FB01  UPON CONS.
*T
     IF    ( JY2-FE01   =   BRK-FE01 ) AND
           ( JY2-FE02   =   BRK-FE02 ) AND
           ( JY2-FE03   =   BRK-FE03 ) AND
           ( JY2-FB09   =   BRK-FB09 )
             ADD        1          TO       CNT-KENSU-D
             IF         JY2-FB01   NOT =    BRK-FB01
                        ADD        1        TO   CNT-MAISU-D
                        MOVE       JY2-FB01 TO   BRK-FB01
             END-IF
*T
*    DISPLAY "  CNT-KENSU-D =" CNT-KENSU-D  UPON CONS
*    DISPLAY "  CNT-MAISU-D =" CNT-MAISU-D  UPON CONS
*T
             GO                   TO   JSMKENL1-WRT-02
     ELSE
             GO                   TO   JSMKENL1-WRT-04
     END-IF.
*
 JSMKENL1-WRT-04.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    BRK-FE01      TO        KEN-F01.
     MOVE    BRK-FE02      TO        KEN-F02.
     MOVE    BRK-FE03      TO        KEN-F03.
     MOVE    BRK-FB09      TO        KEN-F04.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
 JSMKENL1-WRT-05.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    BRK-FE01      TO        KEN-F01.
     MOVE    BRK-FE02      TO        KEN-F02.
     MOVE    BRK-FE03      TO        KEN-F03.
     MOVE    BRK-FB09      TO        KEN-F04.
*---------------------------*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    CNT-MAISU-D   TO        KEN-F11.
     WRITE   KEN-REC.
     MOVE    1             TO        CNT-MAISU-D.
     MOVE    ZERO          TO        CNT-KENSU-D.
     MOVE    JY2-FE01      TO        BRK-FE01.
     MOVE    JY2-FE02      TO        BRK-FE02.
     MOVE    JY2-FE03      TO        BRK-FE03.
     MOVE    JY2-FB09      TO        BRK-FB09.
     MOVE    JY2-FB01      TO        BRK-FB01.
     IF      JY2-END       =         "END"
             GO            TO        JSMKENL1-WRT-EXIT
     ELSE
             GO            TO        JSMKENL1-WRT-03
     END-IF.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    CNT-MAISU-D   TO        KEN-F11.
     REWRITE KEN-REC.
     MOVE    1             TO        CNT-MAISU-D.
     MOVE    ZERO          TO        CNT-KENSU-D.
     MOVE    JY2-FE01      TO        BRK-FE01.
     MOVE    JY2-FE02      TO        BRK-FE02.
     MOVE    JY2-FE03      TO        BRK-FE03.
     MOVE    JY2-FB09      TO        BRK-FB09.
     MOVE    JY2-FB01      TO        BRK-FB01.
     IF      JY2-END       =         "END"
             GO            TO        JSMKENL1-WRT-EXIT
     ELSE
             GO            TO        JSMKENL1-WRT-03
     END-IF.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     CLOSE               AMZJYUL1.
     OPEN      INPUT     AMZJYUL2.
     IF        WRT-CNT    >      ZERO
*              出荷場所件数マスタ出力
               PERFORM   JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     AMZJYUL2  JHSHENL1  AMZJOHL1
               JSMDAYL1  JSMKENL1  TENMS1
               TOKMS2    SHOTBL1   MEIMS1.
*    ＶＬＤ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      WK-TOKCD           TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品名称マスタ読込　　　　　　　　　　　　*
****************************************************************
 MEIMS1-READ-SEC        SECTION.
*
     MOVE   "MEIMS1-READ-SEC"  TO   S-NAME.
*
     READ   MEIMS1
       INVALID
            MOVE   "INV"   TO        MEIMS1-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
