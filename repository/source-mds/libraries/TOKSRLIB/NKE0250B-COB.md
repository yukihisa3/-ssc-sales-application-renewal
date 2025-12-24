# NKE0250B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0250B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　検品システム　　　　　　　　　　　*
*    モジュール名　　　　：　入荷検品データ更新（社内発注分）　*
*    作成日／作成者　　　：　2019/01/29 INOUE                  *
*    処理内容　　　　　　：　倉庫入荷確定データより、　　　　　*
*    　　　　　　　　　　　　振替ファイル、在庫マスタの更新、　*
*                            振替明細、累積ファイル出力を行う。*
*    変更日／作成者　　　：　　　　　　　　　                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NKE0250B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*倉庫入荷確定ファイル
     SELECT      RCVNYKXX    ASSIGN    TO       DA-01-S-RCVNYKXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   RCV-ST.
*商品名称マスタ
     SELECT      MEIMS1    ASSIGN    TO         DA-01-VI-MEIMS1
                           ORGANIZATION         INDEXED
                           ACCESS    MODE       RANDOM
                           RECORD    KEY        MES-F011
                                                MES-F0121
                                                MES-F0122
                                                MES-F0123
                           FILE      STATUS     MES-ST.
*
*仕入先マスタ
*    SELECT      ZSHIMS1     ASSIGN    TO       DA-01-VI-ZSHIMS1
*                            ORGANIZATION       INDEXED
*                            ACCESS    MODE     RANDOM
*                            RECORD    KEY      SHI-F01
*                            FILE      STATUS   SHI-ST.
*
*倉庫マスタ
*    SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
*                            ORGANIZATION       INDEXED
*                            ACCESS    MODE     RANDOM
*                            RECORD    KEY      SOK-F01
*                            FILE      STATUS   SOK-ST.
*
*担当者変換マスタ
     SELECT      TANHENL1    ASSIGN    TO       DA-01-VI-TANHENL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01
                             FILE      STATUS   TAN-ST.
*
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01
                                                JYO-F02
                             FILE      STATUS   SOK-ST.
*社内振替情報ファイル
     SELECT      SFRHEDL5    ASSIGN    TO       DA-01-VI-SFRHEDL5
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      HED-F28
                             FILE      STATUS   HED-ST.
*社内振替明細情報ファイル
     SELECT      SFRMEIL1    ASSIGN    TO       DA-01-VI-SFRMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F01
                                                MEI-F02
                                                MEI-F03
                                                MEI-F04
                                                MEI-F05
                                                MEI-F06
                                                MEI-F07
                                                MEI-F08
                                                MEI-F11
                                                MEI-F12
                             FILE      STATUS   MEI-ST.
*入庫ファイル
*    SELECT      NYKFILL1    ASSIGN    TO       DA-01-VI-NYKFILL1
*                            ORGANIZATION       INDEXED
*                            ACCESS    MODE     RANDOM
*                            RECORD    KEY      NYK-F02
*                                               NYK-F03
*                                               NYK-F04
*                                               NYK-F05
*                            FILE      STATUS   NYK-ST.
*商品在庫マスタ
     SELECT      ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       ZAI-F01
                                               ZAI-F021
                                               ZAI-F022
                                               ZAI-F03
                           FILE      STATUS    ZAI-ST.
*倉庫入荷検品確定累積ファイル
     SELECT   RUINYKL1     ASSIGN    TO        DA-01-VI-RUINYKL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       RUI-F96
                                               RUI-F97
                                               RUI-F01
                                               RUI-F04
                                               RUI-F02
                                               RUI-F03
                           FILE      STATUS    RUI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*倉庫入荷確定ファイル
 FD  RCVNYKXX             LABEL RECORD   IS   STANDARD
     BLOCK                CONTAINS       35   RECORDS.
     COPY        RCVNYKF     OF        XFDLIB
                 JOINING     RCV       PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        HMEIMS      OF        XFDLIB
     JOINING     MES         AS        PREFIX.
*仕入先マスタ
*FD  ZSHIMS1.
*    COPY        ZSHIMS      OF        XFDLIB
*    JOINING     SHI         AS        PREFIX.
*倉庫マスタ
*FD  ZSOKMS1.
*    COPY        ZSOKMS      OF        XFDLIB
*    JOINING     SOK         AS        PREFIX.
*担当者変換マスタ
 FD  TANHENL1.
     COPY        TANHENL1    OF        XFDLIB
     JOINING     TAN         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*社内振替情報ファイル
 FD  SFRHEDL5.
     COPY        SFRHEDL5    OF        XFDLIB
     JOINING     HED         AS        PREFIX.
*社内振替明細情報ファイル
 FD  SFRMEIL1.
     COPY        SFRMEIL1    OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*入庫ファイル
*FD  NYKFILL1.
*    COPY        NYKFILL1    OF        XFDLIB
*    JOINING     NYK         AS        PREFIX.
*商品在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIF     OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
*倉庫入荷検品確定累積ファイル
 FD  RUINYKL1.
     COPY        RUINYKF     OF        XFDLIB
     JOINING     RUI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  RCV-ST              PIC  X(02)  VALUE  SPACE.
     03  MES-ST              PIC  X(02)  VALUE  SPACE.
     03  SHI-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  NYK-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  RUI-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-RUI-F01          PIC  9(07)  VALUE  ZERO.
 01  WK-AREA.
     03  DPNO                PIC  9(07)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  RD-CNT              PIC  9(07)  VALUE  ZERO.
     03  RUI-CNT             PIC  9(07)  VALUE  ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     03  NYK-CNT             PIC  9(07)  VALUE  ZERO.
     03  HED-CNT             PIC  9(07)  VALUE  ZERO.
     03  MEI-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  ZAI-WRT-CNT         PIC  9(07)  VALUE  ZERO.
     03  ZAI-UPD-CNT         PIC  9(07)  VALUE  ZERO.
     03  WK-NYUKA-CHK        PIC  9(07)  VALUE  ZERO.
     03  ERR-KBN             PIC  X(01)  VALUE  SPACE.
**
 01  MEIMS1-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ZSHIMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  TANHENL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  SFRHEDL5-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  SFRMEIL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  ZAMZAIL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  WK-BAK-TANBAN           PIC  X(06)  VALUE  SPACE.
 01  WK-BAK-HNSU             PIC S9(07)  VALUE  ZERO.
 01  WK-KEY-SOKCD            PIC  X(02)  VALUE  SPACE.
 01  WK-KEY-SYOCD            PIC  X(08)  VALUE  SPACE.
 01  WK-KEY-HINCD            PIC  X(08)  VALUE  SPACE.
 01  WK-KEY-TANBAN           PIC  X(06)  VALUE  SPACE.
 01  WK-SURYO                PIC S9(07)  VALUE  ZERO.
 01  WK-SURYOZAN             PIC S9(07)  VALUE  ZERO.
 01  WK-END-EDA              PIC  9(02)  VALUE  ZERO.
 01  CAL-ZAI-F26             PIC S9(08)V99  VALUE  ZERO.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
*
 01  WK-SAGYOUBI-1           PIC  9(08)  VALUE  ZERO.
 01  WK-SAGYOUBI-1-YMD       REDEFINES   WK-SAGYOUBI-1.
     03  WK-SAGYOUBI-Y       PIC  9(04).
     03  WK-SAGYOUBI-M       PIC  9(02).
     03  WK-SAGYOUBI-D       PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
 01  FILLER       REDEFINES  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*
*経理〆日
 01  WK-SIME                 PIC  9(08).
*
*在庫〆日
 01  WK-ZAIKO-SIME.
     03  ZAI-SIME1           PIC  9(08)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(06).
         05  ZAI-SIME1R2     PIC  9(02).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  RCV-ERR            PIC  N(10)  VALUE
                   NC"倉庫入荷検品Ｆ　異常".
     03  MES-ERR             PIC  N(10)  VALUE
                   NC"商品名称Ｍ　　　異常".
     03  SHI-ERR             PIC  N(10)  VALUE
                   NC"仕入先Ｍ　　　　異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫Ｍ　　　　　異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"担当者変換Ｍ　　異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件Ｆ　　　　　異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"発注ＨＥＤＦ　　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"振替明細　Ｆ　　異常".
     03  NYK-ERR             PIC  N(10)  VALUE
                   NC"入庫Ｆ　　　Ｌ５異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫Ｍ　　　Ｌ５異常".
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"倉庫入荷累積Ｆ　異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NKE0250B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NKE0250B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"倉庫入荷確定　読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーあり　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．非社内発注　　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"累積ファイル作成　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"振替情報ファイル更新＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT6.
         05  MSG-OUT6-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT6-FIL2   PIC  N(11)  VALUE
                             NC"振替明細ファイル更新＝".
         05  MSG-OUT06       PIC  ZZZ,ZZ9.
         05  MSG-OUT6-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT6-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT7.
         05  MSG-OUT7-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT7-FIL2   PIC  N(11)  VALUE
                             NC"在庫マスタ更新　　　＝".
         05  MSG-OUT07       PIC  ZZZ,ZZ9.
         05  MSG-OUT7-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT7-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT8.
         05  MSG-OUT8-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT8-FIL2   PIC  N(11)  VALUE
                             NC"在庫マスタ作成　　　＝".
         05  MSG-OUT08       PIC  ZZZ,ZZ9.
         05  MSG-OUT8-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT8-FIL4   PIC  N(01)  VALUE NC"件".
*
 01  WK-RCV-F04              PIC  9(08).
 01  FILLER                  REDEFINES  WK-RCV-F04.
     03  WK-RCV-F04-1        PIC  X(04).
     03  WK-RCV-F04-2        PIC  X(02).
     03  WK-RCV-F04-3        PIC  X(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*伝票番号サブルーチン用ワーク
 01  OUT-DENNO               PIC 9(07).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-IN-SOKCD               PIC  X(02).
 01  LINK-IN-TDATE               PIC  9(08).
 01  LINK-IN-TTIME               PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-SOKCD
                                             LINK-IN-TDATE
                                             LINK-IN-TTIME.
****************************************************************
 DECLARATIVES.
 RCV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVNYKXX.
     DISPLAY     RCV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RCV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MES-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MES-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MES-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
*SHI-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE ZSHIMS1.
*    DISPLAY     SHI-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     SHI-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    STOP        RUN.
*SOK-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
*    DISPLAY     SOK-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     SOK-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TANHENL1.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRHEDL5.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRMEIL1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
*NYK-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE NYKFILL1.
*    DISPLAY     NYK-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     NYK-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 RUI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RUINYKL1.
     DISPLAY     RUI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RUI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        I-O         SFRHEDL5  SFRMEIL1
                             ZAMZAIL1  RUINYKL1.
     OPEN        INPUT       RCVNYKXX   MEIMS1
                             TANHENL1  JYOKEN1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
     DISPLAY "# DATE  = " WK-DATE8     UPON CONS.
     DISPLAY "# TIME  = " WK-TIME-HM   UPON CONS.
*在庫締め日取得
     MOVE      99                 TO   JYO-F01.
     MOVE     "ZAI"               TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF   JYOKEN1-INV-FLG = "INV"
          DISPLAY NC"＃＃条件Ｆ存在無（在庫締日）＃＃" UPON CONS
          MOVE    4000            TO   PROGRAM-STATUS
          STOP    RUN
     ELSE
          MOVE    JYO-F04         TO   ZAI-SIME1
          DISPLAY "# " NC"経理月" " = " ZAI-SIME1R1 " #"
                  UPON CONS
     END-IF.
*倉庫入荷検品ファイル読込
     PERFORM  RCVNYKXX-READ-SEC.
     IF  END-FLG = "END"
          DISPLAY NC"＃＃対象データ無し！！＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    倉庫入荷検品ファイル読込　　　　
****************************************************************
 RCVNYKXX-READ-SEC            SECTION.
     MOVE "RCVNYKXX-READ-SEC"      TO   S-NAME.
*
     READ   RCVNYKXX
            AT  END   MOVE "END"  TO   END-FLG
                      GO          TO   RCVNYKXX-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RD-CNT.
     IF   RD-CNT(5:3)  =  "000" OR "500"
          DISPLAY "# RD-CNT = " RD-CNT " #" UPON CONS
     END-IF.
*発注区分＝2対象（社内発注）
     IF   RCV-F10  NOT =  2
          ADD      1              TO   SKIP-CNT
          GO                      TO   RCVNYKXX-READ-SEC
     END-IF.
*入荷日 数値化
     MOVE  RCV-F04(1:4)           TO   WK-RCV-F04-1.
     MOVE  RCV-F04(6:2)           TO   WK-RCV-F04-2.
     MOVE  RCV-F04(9:2)           TO   WK-RCV-F04-3.
*
 RCVNYKXX-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
* 初期化
     MOVE     SPACE               TO   ERR-KBN.
     MOVE     ZERO                TO   WK-END-EDA.
* 倉庫入荷検品累積Ｆ項目セット
     PERFORM  RUINYKF-SET-SEC.
*
* 各マスタ、ファイルチェック
*    振替情報索引
     MOVE     RCV-F01             TO   HED-F28.
     PERFORM  SFRHEDL5-READ-SEC.
     IF  SFRHEDL5-INV-FLG  =  "INV"
              MOVE  "1"           TO   RUI-F30 ERR-KBN
     ELSE
              MOVE  " "           TO   RUI-F30 ERR-KBN
     END-IF.
     IF  SFRHEDL5-INV-FLG  NOT =  "INV"
         IF   HED-F24   =  "1"
              MOVE  "1"           TO   RUI-F25 ERR-KBN
     ELSE
              MOVE  " "           TO   RUI-F25 ERR-KBN
     END-IF.
*    振替明細索引
     MOVE     HED-F01             TO   MEI-F01.
     MOVE     HED-F02             TO   MEI-F02.
     MOVE     HED-F03             TO   MEI-F03.
     MOVE     HED-F04             TO   MEI-F04.
     MOVE     HED-F05             TO   MEI-F05.
     MOVE     HED-F06             TO   MEI-F06.
     MOVE     HED-F07             TO   MEI-F07.
     MOVE     HED-F08             TO   MEI-F08.
     MOVE     "2"                 TO   MEI-F11.
     MOVE     WK-RCV-F04          TO   MEI-F12.
     PERFORM  SFRMEIL1-READ-SEC.
     IF  SFRMEIL1-INV-FLG  NOT =  "INV"
              MOVE  "1"           TO   RUI-F31 ERR-KBN
*    ELSE
*        既に完了済みの場合
*        IF  MEI-F05 = 1 OR 9
*            MOVE  "1"           TO   RUI-F25 ERR-KBN
*        ELSE
*             入荷数を超えているか？
*             COMPUTE WK-NYUKA-CHK = MEI-F10  +  RCV-F14
*             IF  WK-NYUKA-CHK > MEI-F09
*                 MOVE  "1"       TO   RUI-F26 ERR-KBN
*             END-IF
*        END-IF
     END-IF.
*    商品名称マスタ索引
     MOVE     RCV-F06             TO   MES-F011.
     MOVE     RCV-F07             TO   MES-F0121.
     MOVE     RCV-F08             TO   MES-F0122.
     MOVE     RCV-F09             TO   MES-F0123.
     PERFORM  MEIMS1-READ-SEC.
*    IF  MEIMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   RUI-F27 ERR-KBN
*    END-IF.
*    仕入先マスタ索引
*    MOVE     RCV-F03             TO   SHI-F01.
*    PERFORM  ZSHIMS1-READ-SEC.
*    IF  ZSHIMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   RUI-F28 ERR-KBN
*    END-IF.
*    倉庫マスタ索引
*    MOVE     LINK-IN-SOKCD       TO   SOK-F01.
*    PERFORM  ZSOKMS1-READ-SEC.
*    IF  ZSOKMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   RUI-F29 ERR-KBN
*    END-IF.
*    エラー判定
     IF  ERR-KBN = "1"
         MOVE      "1"            TO   RUI-F22
         ADD        1             TO   ERR-CNT
         GO                       TO   MAIN-010
     ELSE
         MOVE      " "            TO   RUI-F22
     END-IF.
*    各種更新処理へ
*****入庫ファイル作成
*    PERFORM  NYKFILL1-SET-SEC.
*--------------------------------------------------
*****振替情報更新
***  ADD      1                   TO   WK-END-EDA.
***  MOVE     WK-END-EDA          TO   HED-F33.
***  MOVE     WK-DATE8            TO   HED-F99.
***  REWRITE  HED-REC.
***  ADD      1                   TO   HED-CNT.
*****振替明細作成
     INITIALIZE                        MEI-REC.
*    伝票区分
     MOVE  HED-F01                TO   MEI-F01.
*    年度
     MOVE  HED-F02                TO   MEI-F02.
*    シーズン
     MOVE  HED-F03                TO   MEI-F03.
*    倉庫ＣＤ
     MOVE  LINK-IN-SOKCD          TO   MEI-F04.
*    サカタ商品ＣＤ
     MOVE  HED-F05                TO   MEI-F05.
*    サカタ品単１
     MOVE  HED-F06                TO   MEI-F06.
*    サカタ品単２
     MOVE  HED-F07                TO   MEI-F07.
*    サカタ品単３
     MOVE  HED-F08                TO   MEI-F08.
*    商品名１
     MOVE  HED-F09                TO   MEI-F09.
*    商品名２
     MOVE  HED-F10                TO   MEI-F10.
*    発注入荷区分
     MOVE  "2"                    TO   MEI-F11.
*    発注日／入荷日
     MOVE  WK-RCV-F04             TO   MEI-F12.
*    棚番
     MOVE  RCV-F11                TO   MEI-F13.
*    発注数／入荷数
     MOVE  RCV-F14                TO   MEI-F14.
*    ＭＳＧ
     MOVE  NC"検品システムにて入荷"
                                  TO   MEI-F15.
*    入力区分
     MOVE  "1"                    TO   MEI-F91.
*    登録者部門
     MOVE  LINK-IN-BUMON          TO   MEI-F92.
*    登録担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   MEI-F93.
*    登録日付
     MOVE  WK-DATE8               TO   MEI-F94.
*    登録時刻
     MOVE  WK-TIME-HM             TO   MEI-F95.
*    更新者部門   　INITIAL
*    更新担当者ＣＤ INITIAL
*    更新日付       INITIAL
*    更新時刻       INITIAL
*
*    ＷＲＩＴＥ
     WRITE    MEI-REC.
     ADD      1                   TO   MEI-CNT.
*----------------------------------------------------------
*****振替情報　更新
*    入荷数合計
     ADD   RCV-F14                TO   HED-F19.
*    最終入荷担当者部門
     MOVE  LINK-IN-BUMON          TO   HED-F20.
*    最終入荷担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   HED-F21.
*    最終入荷日
     MOVE  WK-RCV-F04             TO   HED-F22.
*    更新者部門
     MOVE  LINK-IN-BUMON          TO   HED-F96.
*    更新担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   HED-F97.
*    更新日付
     MOVE  WK-DATE8               TO   HED-F98.
*    更新時刻
     MOVE  WK-TIME-HM             TO   HED-F99.
*
*    ＲＥＷＲＩＴＥ
     REWRITE  HED-REC.
     ADD      1                   TO   HED-CNT.
*
*--------------------------------------------------------
*****在庫マスタ更新
     PERFORM ZAIKO-SEC.
*
*--------------------------------------------------------
*****累積ファイル＿入荷対象区分・入荷作成区分 セット
     MOVE    "1"                  TO   RUI-F20  RUI-F21.
*
 MAIN-010.
*****倉庫入荷検品確定累積Ｆ作成
     WRITE   RUI-REC.
     ADD     1                    TO   RUI-CNT.
*
     PERFORM  RCVNYKXX-READ-SEC.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               倉庫入荷検品累積Ｆ項目セット
****************************************************************
 RUINYKF-SET-SEC             SECTION.
     MOVE "RUINYKF-SET-SEC"       TO   S-NAME.
*
     MOVE  SPACE                  TO   RUI-REC.
     INITIALIZE                        RUI-REC.
*倉庫ＣＤ
     MOVE  LINK-IN-SOKCD          TO   RUI-F01.
*発注伝票番号
     MOVE  RCV-F01                TO   RUI-F02.
*発注行番号
     MOVE  RCV-F02                TO   RUI-F03.
*仕入先ＣＤ
     MOVE  RCV-F03                TO   RUI-F04.
*ＪＡＮＣＤ
     MOVE  RCV-F05                TO   RUI-F05.
*サカタ商品ＣＤ
     MOVE  RCV-F06                TO   RUI-F06.
*サカタ品単
     MOVE  RCV-F07                TO   RUI-F071.
     MOVE  RCV-F08                TO   RUI-F072.
     MOVE  RCV-F09                TO   RUI-F073.
*発注区分
     MOVE  RCV-F10                TO   RUI-F08.
*_番
     MOVE  RCV-F11                TO   RUI-F09.
*発注数
     MOVE  RCV-F12                TO   RUI-F10.
*発注残数
     MOVE  RCV-F13                TO   RUI-F11.
*入荷数
     MOVE  RCV-F14                TO   RUI-F12.
*入荷日
     MOVE  WK-RCV-F04             TO   RUI-F13.
*完納区分
     MOVE  RCV-F15                TO   RUI-F14.
*不良品数
     MOVE  RCV-F16                TO   RUI-F15.
*不良品区分
     MOVE  RCV-F17                TO   RUI-F16.
*入荷検品担当者ＣＤ
     MOVE  RCV-F19                TO   TAN-F01.
     PERFORM  TANHENL1-READ-SEC.
     IF  TANHENL1-INV-FLG  =  "INV"
         MOVE "01"                TO   RUI-F17  TAN-F03
     ELSE
         MOVE  TAN-F03            TO   RUI-F17
     END-IF.
*入荷検品取込日
*****MOVE  WK-DATE8               TO   RUI-F18  RUI-F96.
     MOVE  WK-DATE8               TO   RUI-F18.
*入荷検品取込時刻
     MOVE  WK-TIME-HM(1:4)        TO   RUI-F19.
*取込日付
     MOVE  LINK-IN-TDATE          TO   RUI-F96.
*取込時刻
     MOVE  LINK-IN-TTIME          TO   RUI-F97.
*担当者部門
     MOVE  LINK-IN-BUMON          TO   RUI-F98.
*取込担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   RUI-F99.
*
 RUINYKF-SET-EXIT.
     EXIT.
****************************************************************
*               入庫ファイル項目セット／更新
****************************************************************
*NYKFILL1-SET-SEC            SECTION.
*    MOVE "NYKFILL1-SET-SEC"      TO   S-NAME.
*
*    MOVE  SPACE                  TO   NYK-REC.
*    INITIALIZE                        NYK-REC.
*伝票区分
*    MOVE  HED-F01                TO   NYK-F01.
*発注伝票番号
*    MOVE  RCV-F01                TO   NYK-F02.
*枝番
*    MOVE  WK-END-EDA             TO   NYK-F03.
*相殺区分
*    MOVE  ZERO                   TO   NYK-F04.
*行番号
*    MOVE  RCV-F02                TO   NYK-F05.
*完了区分
*    MOVE  RCV-F15                TO   NYK-F06.
*量販店伝票番号
*    MOVE  HED-F09                TO   NYK-F07.
*仕入先コード
*    MOVE  HED-F06                TO   NYK-F08.
*取引先コード
*    MOVE  HED-F08                TO   NYK-F09.
*税区分
*    MOVE  HED-F13                TO   NYK-F10.
*送料区分
*    MOVE  HED-F141               TO   NYK-F111.
*送料金額
*    MOVE  HED-F142               TO   NYK-F112.
*納入先
*    MOVE  HED-F16                TO   NYK-F12.
*発注日
*    MOVE  HED-F10                TO   NYK-F13.
*納入予定日
*    MOVE  HED-F11                TO   NYK-F14.
*商品コード
*    MOVE  RCV-F06                TO   NYK-F15.
*品単
*    MOVE  RCV-F07                TO   NYK-F16(1:5).
*    MOVE  RCV-F08                TO   NYK-F16(6:2).
*    MOVE  RCV-F09                TO   NYK-F16(8:1).
*_番
*    IF  RCV-F11 = SPACE
*        MOVE  MEI-F08            TO   NYK-F17
*    ELSE
*        MOVE  RCV-F11            TO   NYK-F17
*    END-IF.
*入庫数
*    MOVE  RCV-F14                TO   NYK-F18.
*単価区分
*    MOVE  SPACE                  TO   NYK-F19.
*仕入単価
*    MOVE  MEI-F12                TO   NYK-F20.
*単価区分
*    MOVE  MEI-F13                TO   NYK-F21.
*原価単価
*    MOVE  MEI-F14                TO   NYK-F22.
*販売単価
*    MOVE  MEI-F15                TO   NYK-F23.
*備考
*    MOVE  "ｿｳｺｹﾝﾋﾟﾝ"             TO   NYK-F24.
*発注残数
*    COMPUTE  NYK-F25  =  MEI-F09  -  MEI-F10.
*実納入日
*    MOVE  RCV-F04(1:4)           TO   WK-RCV-F04-1.
*    MOVE  RCV-F04(6:2)           TO   WK-RCV-F04-2.
*    MOVE  RCV-F04(9:2)           TO   WK-RCV-F04-3.
*    MOVE  WK-RCV-F04             TO   NYK-F26.
*倉庫コード
*    MOVE  LINK-IN-SOKCD          TO   NYK-F27.
*請求区分
*    MOVE  HED-F20                TO   NYK-F28.
*自動発注区分
*    MOVE  ZERO                   TO   NYK-F29.
*計上フラグ
*    MOVE  ZERO                   TO   NYK-F30.
*計上済フラグ
*    MOVE  ZERO                   TO   NYK-F31.
*相手倉庫コード
*    MOVE  SPACE                  TO   NYK-F32.
*担当者
*    MOVE  TAN-F03                TO   NYK-F33.
*支払締日
*    MOVE  WK-ZAIKO-SIME          TO   NYK-F34.
*商品変更区分
*直送請求フラグ
*物流連携区分
*物流連携日
*ユリックス区分
*取消区分
*登録日
*    MOVE  WK-DATE8               TO   NYK-F98.
*更新日
*    MOVE  WK-DATE8               TO   NYK-F99.
*出力
*    WRITE  NYK-REC.
*    ADD    1                     TO   NYK-CNT.
*
*NYKFILL1-SET-EXIT.
*    EXIT.
****************************************************************
*    在庫更新処理
****************************************************************
 ZAIKO-SEC                  SECTION.
*在庫マスタを索引する
     MOVE    LINK-IN-SOKCD  TO   ZAI-F01.
     MOVE    RCV-F06        TO   ZAI-F021.
     MOVE    RCV-F07        TO   ZAI-F022(1:5).
     MOVE    RCV-F08        TO   ZAI-F022(6:2).
     MOVE    RCV-F09        TO   ZAI-F022(8:1).
     MOVE    RCV-F11        TO   ZAI-F03.
     READ    ZAMZAIL1
             INVALID
             PERFORM   ZAIKO-WRITE-SEC
             NOT  INVALID
             PERFORM   ZAIKO-REWRITE-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理１
****************************************************************
 ZAIKO-WRITE-SEC            SECTION.
*在庫マスタレコードを初期化
     MOVE    SPACE          TO   ZAI-REC.
     INITIALIZE                  ZAI-REC.
*キー部セット
     MOVE    LINK-IN-SOKCD  TO   ZAI-F01.
     MOVE    RCV-F06        TO   ZAI-F021.
     MOVE    RCV-F07        TO   ZAI-F022(1:5).
     MOVE    RCV-F08        TO   ZAI-F022(6:2).
     MOVE    RCV-F09        TO   ZAI-F022(8:1).
     MOVE    RCV-F11        TO   ZAI-F03.
*現在庫数
     ADD     RCV-F14        TO   ZAI-F04.
*日付判定
     IF      ZAI-SIME1R1    <  WK-RCV-F04(1:6)
*    翌月分
             ADD   RCV-F14  TO   ZAI-F11
     ELSE
*    当月分
             ADD   RCV-F14  TO   ZAI-F07  ZAI-F06
     END-IF.
*商品名カナセット
     IF   MEIMS1-INV-FLG =  SPACE
          MOVE  MES-F031    TO   ZAI-F30
     ELSE
          MOVE  SPACE       TO   ZAI-F30
     END-IF.
*登録日／更新日セット
     MOVE WK-DATE8          TO   ZAI-F98.
     MOVE WK-DATE8          TO   ZAI-F99.
*
     WRITE  ZAI-REC.
*
     ADD    1               TO   ZAI-WRT-CNT.
*
 ZAIKO-WRITE-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理２
****************************************************************
 ZAIKO-REWRITE-SEC          SECTION.
*
*現在庫数
     ADD     RCV-F14        TO   ZAI-F04.
*未入庫数
     COMPUTE CAL-ZAI-F26    =    ZAI-F26  -  RCV-F14.
     IF      CAL-ZAI-F26    <    0
             MOVE  0        TO   CAL-ZAI-F26
     END-IF.
     MOVE    CAL-ZAI-F26    TO   ZAI-F26.
*日付判定
     IF      ZAI-SIME1R1    <    WK-RCV-F04(1:6)
*    翌月分
             ADD   RCV-F14  TO   ZAI-F11
     ELSE
*    当月分
             ADD   RCV-F14  TO   ZAI-F07  ZAI-F06
     END-IF.
*更新日セット
     MOVE WK-DATE8          TO   ZAI-F99.
*
     REWRITE  ZAI-REC.
*
*
     ADD    1               TO   ZAI-UPD-CNT.
*
 ZAIKO-REWRITE-EXIT.
     EXIT.
***************************************************************
*      ALL       条件ファイルＲＥＡＤ　　　　　　　           *
***************************************************************
 JYOKEN1-READ-SEC           SECTION.
*
     READ    JYOKEN1
       INVALID      KEY
          MOVE     "INV"     TO   JYOKEN1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       振替情報ファイル読込　　                   *
***************************************************************
 SFRHEDL5-READ-SEC      SECTION.
*
     MOVE "SFRHEDL5-READ-SEC" TO  S-NAME.
*
     READ    SFRHEDL5
       INVALID      KEY
          MOVE     "INV"     TO   SFRHEDL5-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   SFRHEDL5-INV-FLG
     END-READ.
*
 SFRHEDL5-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       振替明細ファイル読込　　　                   *
***************************************************************
 SFRMEIL1-READ-SEC      SECTION.
*
     MOVE "SFRMEIL1-READ-SEC" TO  S-NAME.
*
     READ    SFRMEIL1
       INVALID      KEY
          MOVE     "INV"     TO   SFRMEIL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   SFRMEIL1-INV-FLG
     END-READ.
*
 SFRMEIL1-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       商品名称マスタ読込　　　　                   *
***************************************************************
 MEIMS1-READ-SEC        SECTION.
*
     MOVE "MEIMS1-READ-SEC"   TO  S-NAME.
*
     READ    MEIMS1
       INVALID      KEY
          MOVE     "INV"     TO   MEIMS1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       仕入先マスタ読込　　　　　                   *
***************************************************************
*ZSHIMS1-READ-SEC       SECTION.
*
*    MOVE "ZSHIMS1-READ-SEC"  TO  S-NAME.
*
*    READ    ZSHIMS1
*      INVALID      KEY
*         MOVE     "INV"     TO   ZSHIMS1-INV-FLG
*      NOT INVALID  KEY
*         MOVE     SPACE     TO   ZSHIMS1-INV-FLG
*    END-READ.
*
*ZSHIMS1-READ-EXIT.
*    EXIT.
***************************************************************
*      ALL       倉庫マスタ読込　　　　　　                   *
***************************************************************
*ZSOKMS1-READ-SEC       SECTION.
*
*    MOVE "ZSOKMS1-READ-SEC" TO   S-NAME.
*
*    READ    ZSOKMS1
*      INVALID      KEY
*         MOVE     "INV"     TO   ZSOKMS1-INV-FLG
*      NOT INVALID  KEY
*         MOVE     SPACE     TO   ZSOKMS1-INV-FLG
*    END-READ.
*
*ZSOKMS11-READ-EXIT.
*    EXIT.
***************************************************************
*      ALL       担当者変換マスタ読込　　　                   *
***************************************************************
 TANHENL1-READ-SEC      SECTION.
*
     MOVE "TANHENL1-READ-SEC" TO  S-NAME.
*
     READ    TANHENL1
       INVALID      KEY
          MOVE     "INV"     TO   TANHENL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   TANHENL1-INV-FLG
     END-READ.
*
 TANHENL1-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     CLOSE    SFRHEDL5  SFRMEIL1
              ZAMZAIL1  RUINYKL1
              RCVNYKXX   MEIMS1
              JYOKEN1   TANHENL1.
*
*    MOVE     RD-CNT        TO    MSG-OUT01.
*    MOVE     ERR-CNT       TO    MSG-OUT02.
*    MOVE     SKIP-CNT      TO    MSG-OUT03.
*    MOVE     RUI-CNT       TO    MSG-OUT04.
*    MOVE     HED-CNT       TO    MSG-OUT05.
*    MOVE     MEI-CNT       TO    MSG-OUT06.
*    MOVE     ZAI-UPD-CNT   TO    MSG-OUT07.
*    MOVE     ZAI-WRT-CNT   TO    MSG-OUT08.
*    DISPLAY  MSG-OUT1      UPON  CONS.
*    DISPLAY  MSG-OUT2      UPON  CONS.
*    DISPLAY  MSG-OUT3      UPON  CONS.
*    DISPLAY  MSG-OUT4      UPON  CONS.
*    DISPLAY  MSG-OUT5      UPON  CONS.
*    DISPLAY  MSG-OUT6      UPON  CONS.
*    DISPLAY  MSG-OUT7      UPON  CONS.
*    DISPLAY  MSG-OUT8      UPON  CONS.
*    DISPLAY NC"入荷Ｄ読込件数" " = " RD-CNT   UPON CONS.
*    DISPLAY NC"内．エラー件数" " = " ERR-CNT  UPON CONS.
*    DISPLAY NC"内．社外　件数" " = " SKIP-CNT UPON CONS.
*    DISPLAY NC"累積Ｆ作成件数" " = " RUI-CNT  UPON CONS.
*    DISPLAY NC"振替Ｆ更新件数" " = " HED-CNT  UPON CONS.
*    DISPLAY NC"明細Ｆ作成件数" " = " MEI-CNT  UPON CONS.
*    DISPLAY NC"在庫Ｍ更新件数" " = " ZAI-UPD-CNT  UPON CONS.
*    DISPLAY NC"在庫Ｍ作成件数" " = " ZAI-WRT-CNT  UPON CONS.
     DISPLAY NC"入荷Ｄ読込" " = " RD-CNT      "件" UPON CONS.
     DISPLAY NC"内．エラー" " = " ERR-CNT     "件" UPON CONS.
     DISPLAY NC"内．社外　" " = " SKIP-CNT    "件" UPON CONS.
     DISPLAY NC"累積Ｆ作成" " = " RUI-CNT     "件" UPON CONS.
     DISPLAY NC"振替Ｆ更新" " = " HED-CNT     "件" UPON CONS.
     DISPLAY NC"明細Ｆ作成" " = " MEI-CNT     "件" UPON CONS.
     DISPLAY NC"在庫Ｍ更新" " = " ZAI-UPD-CNT "件" UPON CONS.
     DISPLAY NC"在庫Ｍ作成" " = " ZAI-WRT-CNT "件" UPON CONS.
*
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
