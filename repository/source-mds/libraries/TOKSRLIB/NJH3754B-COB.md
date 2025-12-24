# NJH3754B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH3754B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新EDI システム　　　　　　
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ナフコ変換伝票データ作成　　　　　*
*    作成日／更新日　　　：　19/12/05                          *
*    作成者／更新者　　　：　NAV 　　　　　　　　　　　　　　
*    作成日／更新日　　　：　                                  *
*    作成者／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ナフコ発注累積データから当日売上　*
*                            伝票ファイルを作成する。　　　　　*
*                            同時に基本情報ファイルを出力する　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            NJH3754B.
 AUTHOR.                NAV Y.HASHIMOTO.
 DATE-WRITTEN.          19/12/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ナフコ発注累積データ
     SELECT   NFHACL1  ASSIGN    TO        DA-01-VI-NFHACL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-A83
                                            DEN-A88
                                            DEN-A25
                                            DEN-A26
                                            DEN-A22
                                            DEN-A23
                                            DEN-A24
                        FILE      STATUS    DEN-STATUS.
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
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
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*受信件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-STATUS.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F02
                                            JOH-F03
                                            JOH-F04
                                            JOH-F05
                                            JOH-F06
                                            JOH-F07
                                            JOH-F08
                                            JOH-F09
                        FILE      STATUS    JOH-STATUS.
*作場マスタ
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SKB-F01
                        FILE      STATUS    SKB-STATUS.
*作業番号マスタ
     SELECT   NFKANRF   ASSIGN    TO        DA-01-VI-NFKANRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KAR-F01
                                            KAR-F02
                                            KAR-F03
                        FILE      STATUS    KAR-STATUS.
*ナフコ商品名称マスタ
     SELECT   NFSHOMS   ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ナフコ発注累積データ
******************************************************************
 FD  NFHACL1            LABEL RECORD   IS   STANDARD.
*                       BLOCK CONTAINS      15   RECORDS
*                       LABEL RECORD   IS   STANDARD.
*
     COPY     NFHACL1   OF        XFDLIB
              JOINING   DEN       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    出荷場所別件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
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
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
*    作場マスタ
******************************************************************
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SKB       PREFIX.
******************************************************************
*    作業番号ファイル
******************************************************************
 FD  NFKANRF            LABEL RECORD   IS   STANDARD.
     COPY     NFKANRF   OF        XFDLIB
              JOINING   KAR       PREFIX.
******************************************************************
*    ナフコ商品名称マスタ
******************************************************************
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*****************************************************************
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
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  WK-SAIBAN               PIC  9(11)     VALUE  ZERO.
 01  SAKUBAF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFSHOMS-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(05)V9(01)  VALUE  ZERO.
 01  WK-JOH-F21-1            PIC  9(05)     VALUE  ZERO.
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*伝票■編集
 01  HEN-DEN.
     03  HEN-DEN-TEN         PIC  9(02)     VALUE  ZERO.
     03  HEN-DEN-DEN         PIC  9(07)     VALUE  ZERO.
*
 01  WK-HEN-A24.
     03  WK-DEN-A83          PIC  9(03).
     03  FILLER              PIC  X(01)     VALUE  SPACE.
     03  WK-DEN-A87          PIC  X(15).
*
 01  WK-HEN-A28.
     03  FILLER              PIC  X(07)     VALUE  "ｺｳｺｸNO:".
     03  WK-DEN-A81          PIC  9(08).
*
 01  WK-DEN-A56              PIC  9(05)V9(01).
 01  WK-DEN-A56R             REDEFINES      WK-DEN-A56.
     03  WK-HEN-A41          PIC  9(05).
     03  WK-HEN-A42          PIC  9(01).
*
 01  WK-DEN-A63              PIC  9(07)V9(02).
 01  WK-DEN-A63R             REDEFINES      WK-DEN-A63.
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
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SKB-STATUS        PIC  X(02).
     03  KAR-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3754B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3754B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3754B".
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
 01  PARA-SAIBAN            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA
                                       PARA-SAIBAN.
*PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACL1.
     MOVE      "NFHACL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
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
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
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
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUBAF.
     MOVE      "SAKUBAL1"   TO   AB-FILE.
     MOVE      SKB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFKANRF.
     MOVE      "NFKANRL1"   TO   AB-FILE.
     MOVE      KAR-STATUS   TO   AB-STS.
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
     OPEN     I-O       NFHACL1.
     OPEN     INPUT     SHOTBL1   SAKUBAF   NFSHOMS
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1  NFJOHOF   NFKANRF.
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
*管理番号採番
     PERFORM   HJYOKEN-READ-SEC.
*
     READ     NFHACL1
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*
*管理番号(AH09)=0のみ対象とする
     IF    DEN-AH09       =      0
*自社得意先コード
*    得意先マスタ検索
           MOVE    SPACE         TO    TOK-REC
           INITIALIZE                  TOK-REC
           MOVE    DEN-A16       TO    TOK-F01
           READ    TOKMS2
               INVALID
                 MOVE  SPACE     TO    TOK-REC
                 INITIALIZE            TOK-REC
           END-READ
*    商品変換テーブル検索
           MOVE        SPACE     TO    TBL-REC
           INITIALIZE                  TBL-REC
           MOVE        DEN-A16   TO    TBL-F01
           MOVE        DEN-A45   TO    TBL-F02
           READ    SHOTBL1
             INVALID
               MOVE    SPACE     TO    TBL-REC
               INITIALIZE              TBL-REC
           END-READ
           MOVE       TBL-F04    TO    WK-RUTO
           MOVE       DEN-A16    TO    WK-TOKCD
*
*    伝票番号(A23)ブレーク時、出荷場所件数マスタ出力
           IF   WK-DENNO     NOT =     DEN-A23
                PERFORM JSMKENL1-WRT-SEC
                ADD     1         TO   CNT-MAISU
                MOVE    DEN-A23   TO   WK-DENNO
                MOVE    ZERO      TO   CNT-KENSU-D
           END-IF
           ADD       1            TO   CNT-KENSU-D
           ADD       1            TO   CNT-KENSU
*          ファイル出力
           PERFORM      EDIT-SEC
*    ナフコ発注累積データに管理番号を更新
           MOVE      WK-SAIBAN    TO   DEN-AH09
           REWRITE   DEN-REC
     END-IF.
*
     READ     NFHACL1
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
*各量販店領域転送
*    レコード区分
     MOVE     "B"           TO        HEN-A01.
*    データ種別
     MOVE     01            TO        HEN-A02.
*    予備
     MOVE     SPACE         TO        HEN-A03.
*    伝票番号
     MOVE     DEN-A23       TO        HEN-A04.
*    社コード
     MOVE     DEN-A06       TO        HEN-A051.
*    店コード
     MOVE     DEN-A83       TO        HEN-A052.
*    予備
     MOVE     SPACE         TO        HEN-A06.
     MOVE     SPACE         TO        HEN-A07.
*    分類コード
*XXXXMOVE     DEN-A35       TO        HEN-A08.
     MOVE     DEN-A29       TO        HEN-A08.
*    伝票区分
     MOVE     DEN-A22       TO        HEN-A09.
*    発注日
     MOVE     DEN-A25       TO        HEN-A10.
*    納品日
     MOVE     DEN-A26       TO        HEN-A11.
*    取引先コード
     MOVE     DEN-A16       TO        HEN-A12.
*    ステーションアドレス
     MOVE     "00"          TO        HEN-A13.
*    社名
     MOVE     DEN-A10       TO        HEN-A14.
*    店名
     MOVE     DEN-A87       TO        HEN-A15.
*    取引先名
     MOVE     DEN-A20       TO        HEN-A16.
*    取引先電話番号
     MOVE     SPACE         TO        HEN-A17.
*    発注区分
     MOVE     DEN-A28       TO        HEN-A18.
*    発注区分
     MOVE     DEN-A88       TO        HEN-A181.
*    予備
     MOVE     SPACE         TO        HEN-A19.
*    伝票ヘッダーオプションラインコード
     MOVE     0             TO        HEN-A20.
*    レコード区分
     MOVE     "C"           TO        HEN-A21.
*    データ種別
     MOVE     01            TO        HEN-A22.
*    Ａ欄
     MOVE     DEN-A23       TO        HEN-DEN-DEN.
     MOVE     HEN-DEN-DEN   TO        HEN-A23.
*    Ｄ欄の上段
     MOVE     DEN-A83       TO        WK-DEN-A83.
     MOVE     DEN-A87       TO        WK-DEN-A87.
     MOVE     WK-HEN-A24    TO        HEN-A24.
*    Ｄ欄の下段左
     MOVE     SPACE         TO        HEN-A25.
*    Ｄ欄の下段右
     IF    DEN-A29          =    40
           MOVE   "ｷｬｸﾁｭｳ"  TO        HEN-A26
     END-IF.
*    Ｅ欄
     MOVE     SPACE         TO        HEN-A27.
*    Ｆ欄の下段
     IF    DEN-A29          =    30
           MOVE  DEN-A81    TO        WK-DEN-A81
           MOVE  WK-HEN-A28 TO        HEN-A28
     END-IF.
*    Ｆ欄の上段
     MOVE     SPACE         TO        HEN-A29.
*    Ｌ欄の中段
     MOVE     SPACE         TO        HEN-A30.
*    Ｌ欄の上段
     MOVE     SPACE         TO        HEN-A31.
*    Ｌ欄の下段
     MOVE     "**EOS**"     TO        HEN-A32.
*    伝票ヘッダーオプションラインコード
     MOVE     0             TO        HEN-A33.
*    レコード区分
     MOVE     "D"           TO        HEN-A34.
*    データ種別
     MOVE     01            TO        HEN-A35.
*    行番号
     MOVE     DEN-A24       TO        HEN-A36.
*    ＪＡＮ商品コード
     MOVE     DEN-A44       TO        HEN-A37.
*    柄
     MOVE     SPACE         TO        HEN-A38.
*    予備
     MOVE     SPACE         TO        HEN-A39.
*    単位
     MOVE     DEN-A61       TO        HEN-A40.
*    数量（発注数整数部）
     MOVE     DEN-A56       TO        WK-DEN-A56.
     MOVE     WK-HEN-A41    TO        HEN-A41.
*    予備（発注数小数部）
     MOVE     WK-HEN-A42    TO        HEN-A42.
*    原単価
     MOVE     ZERO          TO        WK-DEN-A63.
     IF    DEN-A62          =    0
           MOVE  DEN-A63    TO        WK-DEN-A63
     END-IF.
     IF    DEN-A62          =    1
           MOVE  DEN-A64    TO        WK-DEN-A63
     END-IF.
     MOVE     WK-HEN-A43    TO        HEN-A43.
*    予備
     MOVE     WK-HEN-A44    TO        HEN-A44.
*    売単価
     EVALUATE  DEN-A68
         WHEN  1
         WHEN  3
               MOVE   DEN-A70    TO   HEN-A45
         WHEN  2
               MOVE   DEN-A71    TO   HEN-A45
     END-EVALUATE.
*    原価金額
     EVALUATE  DEN-A62
         WHEN  0
               MOVE   DEN-A65    TO   HEN-A46
         WHEN  1
               MOVE   DEN-A66    TO   HEN-A46
     END-EVALUATE.
*    売価金額（小数点以下切捨て）
     EVALUATE  DEN-A68
         WHEN  1
         WHEN  3
               COMPUTE HEN-A47  =  DEN-A70 * DEN-A56
         WHEN  2
               COMPUTE HEN-A47  =  DEN-A71 * DEN-A56
     END-EVALUATE.
*    予備
     MOVE     SPACE         TO        HEN-A48.
*    商品名
     MOVE     DEN-A51       TO        HEN-A49.
*    発注者商品コード
     MOVE     DEN-A45       TO        HEN-A50.
*    色
     MOVE     SPACE         TO        HEN-A51.
*    サイズ
     MOVE     SPACE         TO        HEN-A52.
*    予備
     MOVE     SPACE         TO        HEN-A53.
*    伝票明細オプションラインコード
     MOVE     ZERO          TO        HEN-A54.
*    レコード区分
     MOVE     "E"           TO        HEN-A55.
*    データ種別
     MOVE     01            TO        HEN-A56.
*    規格
     MOVE     DEN-A55       TO        HEN-A57.
*    プライスシール発行区分
     MOVE     SPACE         TO        HEN-A58.
*    仕入先印刷区分
     MOVE     SPACE         TO        HEN-A59.
*    部門コード
*XXXXMOVE     ZERO          TO        HEN-A60.
     MOVE     DEN-A35       TO        HEN-A60.
*    ＪＡＮ商品コード
     MOVE     SPACE         TO        HEN-A61.
*    商品名略
     MOVE     SPACE         TO        HEN-A62.
*    規格略
     MOVE     SPACE         TO        HEN-A63.
*    売単価
     MOVE     ZERO          TO        HEN-A64.
*    出力枚数
     MOVE     ZERO          TO        HEN-A65.
*    予備
     MOVE     SPACE         TO        HEN-A66.
*    伝票明細オプションラインコード
     MOVE     1             TO        HEN-A67.
*
     PERFORM  TENSO-SEC.
*当日売上伝票ファイル更新
     WRITE    HEN-REC.
*基本情報ファイル更新
     WRITE    JOH-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*管理番号
     MOVE        WK-SAIBAN TO        JOH-F01.
*取引先コード
     MOVE        HEN-A12   TO        HEN-F01.
*    MOVE        HEN-A12   TO        WK-TOKCD   JOH-F04.
     MOVE        HEN-A12   TO        JOH-F04.
*伝票■
     MOVE        HEN-A04   TO        HEN-F02
                                     HEN-F23    JOH-F07.
*行■
     MOVE        HEN-A36   TO        HEN-F03    JOH-F08.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        HEN-A052  TO        HEN-F07    JOH-F06.
*  商品変換テーブル検索
*    MOVE        SPACE     TO        TBL-REC.
*    INITIALIZE                      TBL-REC.
*    MOVE        HEN-F01   TO        TBL-F01.
*    MOVE        HEN-A50   TO        TBL-F02.
*    READ    SHOTBL1
*      INVALID
*        MOVE    SPACE     TO        TBL-REC
*        INITIALIZE                  TBL-REC
*    END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09    JOH-F28.
*注文番号
     MOVE        SPACE     TO        HEN-F10.
*振分ルート取得
*    MOVE       TBL-F04    TO        WK-RUTO.
*    MOVE       HEN-F02    TO        WK-DENNO.
*---------------------------*
*発注日
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A10   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F111
     ELSE
         MOVE    ZERO           TO   HEN-F111
     END-IF.
*納品日
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A11   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F112   JOH-F09
     ELSE
         MOVE    ZERO           TO   HEN-F112   JOH-F09
     END-IF.
*出荷日
     MOVE        ZERO      TO        HEN-F113.
*分類コード
     MOVE        HEN-A08   TO        HEN-F12.
*商品区分
     MOVE        SPACE     TO        HEN-F131.
*伝票区分
     MOVE        HEN-A09   TO        HEN-F132.
*請求
     MOVE        ZERO      TO        HEN-F133.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411  JOH-F15.
*自社商品単品コード
     MOVE        TBL-F032  TO        HEN-F1412  JOH-F16.
*商品名
     MOVE        HEN-A49(1:15)    TO   HEN-F1421.
     MOVE        HEN-A49(16:10)   TO   HEN-F1422.
*数量
     MOVE        HEN-A41   TO        HEN-F15    JOH-F19  JOH-F20.
*単
     MOVE       "1"        TO        HEN-F16.
*仕入単価
     MOVE        ZERO      TO        HEN-F171.
*原価単価
     MOVE        HEN-A43   TO        HEN-F172.
*売価単価
     MOVE        HEN-A45   TO        HEN-F173.
*原価金額
     MOVE        HEN-A46   TO        HEN-F181.
*売価金額
     MOVE        HEN-A47   TO        HEN-F182.
*消費税
     MOVE        ZERO      TO        HEN-F19.
*粗利
     MOVE        ZERO      TO        HEN-F20.
*ストック番号
     MOVE        SPACE     TO        HEN-F21.
*備考
     MOVE        SPACE     TO        HEN-F22.
*自社得意先コード
*    IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
*        MOVE    SPACE         TO    TOK-REC
*        INITIALIZE                  TOK-REC
*        MOVE    HEN-F01       TO    TOK-F01
*        READ    TOKMS2
*            INVALID
*              MOVE  SPACE     TO    TOK-REC
*              INITIALIZE            TOK-REC
*        END-READ
*        MOVE    TOK-F52   TO        HEN-F24
*        MOVE    1         TO        FLG-TOK
*    ELSE
*        MOVE    TOK-F52   TO        HEN-F24
*    END-IF.
     MOVE        TOK-F52   TO        HEN-F24.
*相手商品コード
     MOVE        HEN-A50   TO        HEN-F25    JOH-F13.
     MOVE        HEN-A61   TO        JOH-F14.
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
     MOVE        9         TO        HEN-F276.
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
     MOVE        HEN-A15   TO        HEN-F30.
*伝票枚数
     MOVE        ZERO      TO        HEN-F98.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*オンライン伝票枚数
     MOVE        ZERO      TO        HEN-F40.
*ルート
*    MOVE        WK-RUTO   TO        HEN-F42.
     MOVE        TBL-F04   TO        HEN-F42.
*ストックナンバー
     MOVE        SPACE     TO        HEN-F43.
*注文番号（新）
     MOVE        SPACE     TO        HEN-F44.
*売上計上日
     MOVE        ALL "0"   TO        HEN-F45.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46  JOH-F02.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47  JOH-F03.
*振分倉庫コード
     IF  TBL-F04  NOT =  SPACE
         MOVE  TBL-F04   TO          HEN-F48  JOH-F05
     ELSE
         MOVE  TOK-F81   TO          HEN-F48  JOH-F05
     END-IF.
*作場マスタ索引
     MOVE   JOH-F05        TO        SKB-F01.
     PERFORM  SAKUBAF-READ-SEC.
     IF     SAKUBAF-INV-FLG = "INV"
            MOVE  "1"      TO        JOH-F12
     ELSE
            MOVE  SKB-F03  TO        JOH-F12
     END-IF.
*棚番
     MOVE        TBL-F08   TO        HEN-F49    JOH-F29.
*訂正前数量
     MOVE        HEN-A41   TO        HEN-F50.
*訂正仕入単価
     MOVE        ZERO      TO        HEN-F511.
*修正原価単価
     MOVE        HEN-A43   TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A45   TO        HEN-F513.
*修正原価金額
     MOVE        HEN-A46   TO        HEN-F521.
*修正売価金額
     MOVE        HEN-A47   TO        HEN-F522.
*訂正フラグ
     MOVE        ZERO      TO        HEN-F53.
*更新済フラグ
     MOVE        ZERO      TO        HEN-F54.
*在庫引当フラグ
     MOVE        ZERO      TO        HEN-F55.
*出荷完了FLG
     MOVE        ZERO      TO        HEN-F56.
*オンライン／手書区分
     MOVE        "1"       TO        JOH-F17.
*ナフコ商品名称マスタ読込
     MOVE        JOH-F13   TO        MEI-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG = SPACE
         COMPUTE WK-JOH-F21   = JOH-F19 / MEI-F09 / MEI-F10
         COMPUTE WK-JOH-F21-1 = WK-JOH-F21 + 0.9
         MOVE    WK-JOH-F21-1 TO   JOH-F21  JOH-F22
         MOVE    MEI-F09      TO   JOH-F18
     ELSE
         MOVE    JOH-F19      TO   JOH-F21  JOH-F22
     END-IF.
*登録担当者部門
     MOVE        "2940"    TO        JOH-F30.
*登録担当者ＣＤ
     MOVE        "99"      TO        JOH-F31.
*データ転送
*    MOVE    WK-DEPB-REC   TO        JOH-HE00.
*JOH-HE00.
*レコード区分
     MOVE    HEN-A01       TO        JOH-HE01.
*データ種別
     MOVE    HEN-A02       TO        JOH-HE02.
*予備
     MOVE    HEN-A03       TO        JOH-HE03.
*伝票番号
     MOVE    HEN-A04       TO        JOH-HE04.
*社コード
     MOVE    HEN-A051      TO        JOH-HE05.
*取引先コード
     MOVE    HEN-A052      TO        JOH-HE06.
*予備
     MOVE    HEN-A06       TO        JOH-HE07.
*予備
     MOVE    HEN-A07       TO        JOH-HE08.
*分類コード
     MOVE    HEN-A08       TO        JOH-HE09.
*伝票区分
     MOVE    HEN-A09       TO        JOH-HE10.
*発注日
     MOVE    HEN-A10       TO        JOH-HE11.
*納品日
     MOVE    HEN-A11       TO        JOH-HE12.
*取引先ＣＤ
     MOVE    HEN-A12       TO        JOH-HE13.
*ステーションアドレス
     MOVE    HEN-A13       TO        JOH-HE14.
*社名
     MOVE    HEN-A14       TO        JOH-HE15.
*店名
     MOVE    HEN-A15       TO        JOH-HE16.
*取引先名
     MOVE    HEN-A16       TO        JOH-HE17.
*取引先電話番号
     MOVE    HEN-A17       TO        JOH-HE18.
*発注区分
     MOVE    HEN-A18       TO        JOH-HE19.
*納品場所
     MOVE    HEN-A181      TO        JOH-HE20.
*予備
     MOVE    HEN-A19       TO        JOH-HE21.
*伝票ヘッダオプションライン
     MOVE    HEN-A20       TO        JOH-HE22.
*    MOVE    WK-DEPC-REC   TO        JOH-HF00.
*JOH-HF00.
*レコード区分
     MOVE    HEN-A21       TO        JOH-HF01.
*データ種別
     MOVE    HEN-A22       TO        JOH-HF02.
*Ａ欄
     MOVE    HEN-A23       TO        JOH-HF03.
*Ｄ欄の上段
     MOVE    HEN-A24       TO        JOH-HF04.
*Ｄ欄の下段左
     MOVE    HEN-A25       TO        JOH-HF05.
*Ｄ欄の下段右
     MOVE    HEN-A26       TO        JOH-HF06.
*Ｅ欄
     MOVE    HEN-A27       TO        JOH-HF07.
*Ｆ欄の下段
     MOVE    HEN-A28       TO        JOH-HF08.
*Ｆ欄の上段
     MOVE    HEN-A29       TO        JOH-HF09.
*Ｌ欄の中段
     MOVE    HEN-A30       TO        JOH-HF10.
*Ｌ欄の上段
     MOVE    HEN-A31       TO        JOH-HF11.
*ルート
*XXXXMOVE    SPACE         TO        JOH-HF12.
     MOVE    HEN-F42       TO        JOH-HF12.
*Ｌ欄の下段
     MOVE    HEN-A32       TO        JOH-HF13.
*伝票ヘッダオプションライン
     MOVE    HEN-A33       TO        JOH-HF14.
*    MOVE    WK-DEPD-REC   TO        JOH-ME00.
*JOH-ME00.
*レコード区分
     MOVE    HEN-A34       TO        JOH-ME01.
*データ種別
     MOVE    HEN-A35       TO        JOH-ME02.
*行番号
     MOVE    HEN-A36       TO        JOH-ME03.
*ＪＡＮ商品コード
     MOVE    HEN-A37       TO        JOH-ME04.
*柄
     MOVE    HEN-A38       TO        JOH-ME05.
*予備
     MOVE    HEN-A39       TO        JOH-ME06.
*単位
     MOVE    HEN-A40       TO        JOH-ME07.
*数量
     MOVE    HEN-A41       TO        JOH-ME08.
*予備
     MOVE    HEN-A42       TO        JOH-ME09.
*原単価
     MOVE    HEN-A43       TO        JOH-ME10.
*予備
     MOVE    HEN-A44       TO        JOH-ME11.
*売単価
     MOVE    HEN-A45       TO        JOH-ME12.
*原価金額
     MOVE    HEN-A46       TO        JOH-ME13.
*売価金額
     MOVE    HEN-A47       TO        JOH-ME14.
*予備
     MOVE    HEN-A48       TO        JOH-ME15.
*商品名
     MOVE    HEN-A49       TO        JOH-ME16.
*発注商品コード
     MOVE    HEN-A50       TO        JOH-ME17.
*色
     MOVE    HEN-A51       TO        JOH-ME18.
*サイズ
     MOVE    HEN-A52       TO        JOH-ME19.
*予備
     MOVE    HEN-A53       TO        JOH-ME20.
*明細ラインコード
     MOVE    HEN-A54       TO        JOH-ME21.
*    MOVE    WK-DEPE-REC   TO        JOH-MF00.
*JOH-MF00.
*レコード区分
     MOVE    HEN-A55       TO        JOH-MF01.
*データ種別
     MOVE    HEN-A56       TO        JOH-MF02.
*規格
     MOVE    HEN-A57       TO        JOH-MF03.
*プライスシール発行区分
     MOVE    HEN-A58       TO        JOH-MF04.
*仕入先印刷区分
     MOVE    HEN-A59       TO        JOH-MF05.
*部門コード
     MOVE    HEN-A60       TO        JOH-MF06.
*ＪＡＮ商品コード
     MOVE    HEN-A61       TO        JOH-MF07.
*商品名略
     MOVE    HEN-A62       TO        JOH-MF08.
*規格名略
     MOVE    HEN-A63       TO        JOH-MF09.
*売単価
     MOVE    HEN-A64       TO        JOH-MF10.
*出力枚数
     MOVE    HEN-A65       TO        JOH-MF11.
*予備
     MOVE    HEN-A66       TO        JOH-MF12.
*伝票明細オプションライン
     MOVE    HEN-A67       TO        JOH-MF13.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD      TO        KEN-F03.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*---------------------------*
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD      TO        KEN-F03.
*ルートＣＤのセット
     IF    WK-RUTO  =  SPACE
           MOVE  TOK-F81   TO        KEN-F04
     ELSE
           MOVE  WK-RUTO   TO        KEN-F04
     END-IF.
*---------------------------*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
*    MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     IF      END-FG    NOT =      9
             ADD     CNT-KENSU-D  TO KEN-F10
             ADD     1            TO KEN-F11
     END-IF.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        WRT-CNT    >      ZERO
*              出荷場所件数マスタ出力
               PERFORM   JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
*              ナフコ管理番号ファイル
               PERFORM   NFKANRF-WT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     NFHACL1  JHSHENL1
               SHOTBL1
               JHMRUTL1  TOKMS2    NFSHOMS
               JSMKENL1  JSMDAYL1  NFKANRF  SAKUBAF NFJOHOF.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     CLOSE     VLD500.
*
     MOVE      WK-SAIBAN TO        PARA-SAIBAN.
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
*　　　　　　　条件ファイル読込　　　　　　　　　　　　　　　　*
****************************************************************
 HJYOKEN-READ-SEC        SECTION.
*
     MOVE   "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     OPEN   I-O   HJYOKEN.
*
     MOVE   "99"           TO        JYO-F01.
     MOVE   "NAFUKO"       TO        JYO-F02.
     READ   HJYOKEN
       INVALID
            DISPLAY "## HJYOKEN INVALID KEY 99 NAFUKO" UPON CONS
            STOP  RUN
     END-READ.
*
     ADD    1              TO        JYO-F04.
*
     IF     JYO-F04  >  JYO-F06
            MOVE  JYO-F05  TO        JYO-F04
            MOVE  JYO-F04  TO        WK-SAIBAN
     ELSE
            MOVE  JYO-F04  TO        WK-SAIBAN
     END-IF.
*
     REWRITE JYO-REC.
*
     CLOSE  HJYOKEN.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　作場マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 SAKUBAF-READ-SEC        SECTION.
*
     MOVE   "SAKUBAF-READ-SEC"  TO   S-NAME.
*
     READ   SAKUBAF
       INVALID
            MOVE   "INV"   TO        SAKUBAF-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ商品名称マスタ読込　　　　　　　　　　　　*
****************************************************************
 NFSHOMS-READ-SEC        SECTION.
*
     MOVE   "NFSHOMS-READ-SEC"  TO   S-NAME.
*
     READ   NFSHOMS
       INVALID
            MOVE   "INV"   TO        NFSHOMS-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ管理番号ファイル作成　　　　　　　　　　　*
****************************************************************
 NFKANRF-WT-SEC          SECTION.
*
     MOVE   "NFKANRF-WT-SEC"    TO   S-NAME.
*
     MOVE    SPACE              TO   KAR-REC.
     INITIALIZE                      KAR-REC.
*
     MOVE    "1"                TO   KAR-F01.
     MOVE    WK-SAIBAN          TO   KAR-F02.
     MOVE    PARA-JDATE         TO   KAR-F031.
     MOVE    PARA-JTIME         TO   KAR-F032.
     MOVE    WK-TOKCD           TO   KAR-F033.
     MOVE    "99"               TO   KAR-F04.
     MOVE    SYS-DATEW          TO   KAR-F98.
     MOVE    WK-TIME(1:6)       TO   KAR-F99.
*
     WRITE   KAR-REC.
*
 NFKANRF-WT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
