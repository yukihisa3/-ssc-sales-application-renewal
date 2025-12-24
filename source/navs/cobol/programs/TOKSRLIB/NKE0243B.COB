****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　検品システム　　　　　　　　　　　*
*    モジュール名　　　　：　入荷検品データ更新３メーカー発注　*
*    作成日／作成者　　　：　2019/01/28 T.TAKAHASHI            *
*    処理内容　　　　　　：　ＷＫ発注ヘッダ／明細Ｆを読み、発注*
*    　　　　　　　　　　　　ヘッダ／明細Ｆ、入庫Ｆ、在庫マスタ*
*                            の更新を行なう。　　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NKE0243B.
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
*ＷＫ発注ヘッダファイル
     SELECT      WHCHEDL1    ASSIGN    TO       DA-01-VI-WHCHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      WCH-F02
                             FILE      STATUS   WCH-ST.
*ＷＫ発注明細ファイル
     SELECT      WHCMEIL1    ASSIGN    TO       DA-01-VI-WHCMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      WCM-A01
                                                WCM-A02
                             FILE      STATUS   WCM-ST.
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
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01
                                                JYO-F02
                             FILE      STATUS   SOK-ST.
*発注ヘッダファイル
     SELECT      HACHEDL1    ASSIGN    TO       DA-01-VI-HACHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      HED-F02
                             FILE      STATUS   HED-ST.
*発注明細ファイル
     SELECT      HACMEIL1    ASSIGN    TO       DA-01-VI-HACMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F02
                                                MEI-F03
                             FILE      STATUS   MEI-ST.
*入庫ファイル
     SELECT      NYKFILL1    ASSIGN    TO       DA-01-VI-NYKFILL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      NYK-F02
                                                NYK-F03
                                                NYK-F04
                                                NYK-F05
                             FILE      STATUS   NYK-ST.
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
*ＷＫ発注ヘッダファイル
 FD  WHCHEDL1.
     COPY        WHCHEDF     OF        XFDLIB
     JOINING     WCH         AS        PREFIX.
*ＷＫ発注明細ファイル
 FD  WHCMEIL1.
     COPY        WHCMEIF     OF        XFDLIB
     JOINING     WCM         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        HMEIMS      OF        XFDLIB
     JOINING     MES         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*発注ヘッダファイル
 FD  HACHEDL1.
     COPY        HACHEDF     OF        XFDLIB
     JOINING     HED         AS        PREFIX.
*発注明細ファイル
 FD  HACMEIL1.
     COPY        HACMEIF     OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*入庫ファイル
 FD  NYKFILL1.
     COPY        NYKFILL1    OF        XFDLIB
     JOINING     NYK         AS        PREFIX.
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
     03  WCH-ST              PIC  X(02)  VALUE  SPACE.
     03  WCM-ST              PIC  X(02)  VALUE  SPACE.
     03  MES-ST              PIC  X(02)  VALUE  SPACE.
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
     03  WK-NYUKA-CHK        PIC  9(07)  VALUE  ZERO.
     03  MEI-FLG             PIC  X(01)  VALUE  SPACE.
**
 01  MEIMS1-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  ZSHIMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS1-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  TANHENL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  HACHEDL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  HACMEIL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
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
 01  WK-HAC-ZAN              PIC S9(06)  VALUE  ZERO.
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
     03  WCH-ERR            PIC  N(10)  VALUE
                   NC"ＷＫ発注ヘッダＦ異常".
     03  WCM-ERR            PIC  N(10)  VALUE
                   NC"ＷＫ発注明細Ｆ　異常".
     03  MES-ERR             PIC  N(10)  VALUE
                   NC"商品名称Ｍ　　　異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件Ｆ　　　　　異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"発注ＨＥＤＦ　　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"発注明細　Ｆ　　異常".
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
         05  ST-PG           PIC  X(08)  VALUE "NKE0243B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NKE0243B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"振替ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"振替ファイル作成　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"明細ファイル作成　　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"ヘッダファイル更新　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"明細ファイル更新　　＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
*
 01  WK-RUI-F13              PIC  9(08).
 01  FILLER                  REDEFINES  WK-RUI-F13.
     03  WK-RUI-F13-1        PIC  X(04).
     03  WK-RUI-F13-2        PIC  X(02).
     03  WK-RUI-F13-3        PIC  X(02).
*
 01  WK-NYK-YMD.
     01  WK-NYK-YYYY         PIC  9(04)  VALUE ZERO.
     01  WK-NYK-MM           PIC  9(02)  VALUE ZERO.
     01  WK-NYK-DD           PIC  9(02)  VALUE ZERO.
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
 01  LINK-IN-KENSU               PIC  9(07).
 01  LINK-IN-TORIDT              PIC  9(08).
 01  LINK-IN-TORITM              PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-SOKCD
                                             LINK-IN-KENSU
                                             LINK-IN-TORIDT
                                             LINK-IN-TORITM.
****************************************************************
 DECLARATIVES.
 WCH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE WHCHEDL1.
     DISPLAY     WCH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 WCM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE WHCMEIL1.
     DISPLAY     WCM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     WCM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MES-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MES-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MES-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACHEDL1.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACMEIL1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 NYK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NYKFILL1.
     DISPLAY     NYK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NYK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
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
     OPEN        I-O         HACHEDL1  HACMEIL1  NYKFILL1
                             ZAMZAIL1  RUINYKL1.
     OPEN        INPUT       RCVNYKF   MEIMS1    ZSHIMS1
                             ZSOKMS1   JYOKEN1   TANHENL1.
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
     PERFORM WHCHEDL1-READ-SEC.
     IF  END-FLG = "END"
          DISPLAY NC"＃＃対象データ無し！！＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    ＷＫ発注ヘッダファイル読込
****************************************************************
 WHCHEDL1-READ-SEC           SECTION.
     MOVE "WHCHEDL1-READ-SEC"     TO   S-NAME.
*
     READ   WHCHEDL1
            AT  END   MOVE "END"  TO   END-FLG
                      GO          TO   WHCHEDL1-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RD-CNT.
     IF   RD-CNT(5:3)  =  "000" OR "500"
          DISPLAY "# RD-CNT = " RD-CNT " #" UPON CONS
     END-IF.
*エラー区分＝１以外が対象
     IF   WHC-F41  =  "1"
          ADD      1              TO   SKIP-CNT
          GO                      TO   WHCHEDL1-READ-SEC
     END-IF.
*発注ヘッダ索引
     MOVE   WCH-F02               TO   HED-F02.
     PERFORM  HACHEDL1-READ-SEC.
     IF     HACHEDL1-INV-FLG  =  "INV"
          GO                      TO   WHCHEDL1-READ-SEC
     END-IF.
*
     MOVE     SPACE               TO   MEI-FLG.
*
 WHCHEDL1-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
* ＷＫ発注明細読込（スタート）
     MOVE     SPACE               TO   WCM-REC.
     INITIALIZE                        WCM-REC.
     MOVE     WCH-F02             TO   WCM-A01.
     START  WHCMEIL1  KEY  IS  >=  WCM-A01  WCM-A02
            INVALID
            DISPLAY NC"＃ＷＫ発注明細対象無！＃＃" WCM-A01 " - "
                    WCM-A02  UPON CONS
            GO                    TO   MAIN-010
     END-START.
*
 MAIN-000.
*****ＷＫ発注明細読込
     READ   WHCMEIL1
            NEXT  AT  END
            GO                    TO   MAIN-010
     END-READ.
*発注ヘッダ索引
     MOVE   WCM-A01               TO   MEI-F02.
     MOVE   WCM-A02               TO   MEI-F03.
     PERFORM  HACMEIL1-READ-SEC.
     IF     HACMEIL1-INV-FLG  =  "INV"
          GO                      TO   MAIN-010
     END-IF.
*****伝票番号ブレイクチェック
     IF     WCH-F02   NOT =  WCM-A01
            GO                    TO   MAIN-010
     END-IF.
*****最終枝番取得
     MOVBE  HED-F33               TO   WK-END-EDA.
*****更新判定
*****（残数計算）
     COMPUTE   WK-HAC-ZAN  =  MEI-F09  -  MEI-F10.
*＜更新１＞
*******ＷＫ発注明細＝０で発注残がない場合、レコード作成しない
     IF   (  WCM-B01     =   0  )
     AND  (  WK-HAC-ZAN  <=  0  OR  WCM-F05  =  "1"  )
**********この場合はなにもしない
             GO                    TO   MAIN-000
     END-IF.
*＜更新２＞
*******ＷＫ発注明細＝０で発注残がある場合、レコード作成しない
     IF   (  WCM-B01     =   0  )
     AND  (  WK-HAC-ZAN  >   0  OR  WCM-F05  =  " "  )
**********入庫ファイルを作成する
             PERFORM  NYKFILL1-SET-SEC
**********発注明細更新
             IF   WCM-B12  =  "1"
                  MOVE         1          TO   MEI-F05
             END-IF
             ADD   WCM-B11                TO   MEI-F10
             MOVE  WCM-B04(1:4)           TO   WK-RUI-F13-1
             MOVE  WCM-B04(6:2)           TO   WK-RUI-F13-2
             MOVE  WCM-B04(9:2)           TO   WK-RUI-F13-3
             MOVE  WK-RUI-F13             TO   MEI-F17
             MOVE  WK-DATE8               TO   MEI-F99
             REWRITE  MEI-REC
             ADD      1                   TO   MEI-CNT
             MOVE    "1"                  TO   MEI-FLG
             GO                    TO   MAIN-000
     END-IF.
*＜更新３＞
*******ＷＫ発注明細≠０
     IF   (  WCM-B01     NOT =   0  )
**********入庫ファイルを作成する
             PERFORM  NYKFILL1-SET-SEC
**********発注明細更新
             IF   WCM-B12  =  "1"
                  MOVE         1          TO   MEI-F05
             END-IF
             ADD   WCM-B11                TO   MEI-F10
             MOVE  WCM-B04(1:4)           TO   WK-RUI-F13-1
             MOVE  WCM-B04(6:2)           TO   WK-RUI-F13-2
             MOVE  WCM-B04(9:2)           TO   WK-RUI-F13-3
             MOVE  WK-RUI-F13             TO   MEI-F17
             MOVE  WK-DATE8               TO   MEI-F99
             REWRITE  MEI-REC
             ADD      1                   TO   MEI-CNT
             MOVE    "1"                  TO   MEI-FLG
*************倉庫入荷検品確定累積ファイル更新
             MOVE  LINK-IN-TORIDT         TO   RUI-F96
             MOVE  LINK-IN-TORITM         TO   RUI-F97
             MOVE  LINK-IN-SOKCD          TO   RUI-F01
             MOVE  WCM-B03                TO   RUI-F04
             MOVE  WCM-A01                TO   RUI-F02
             MOVE  WCM-A02                TO   RUI-F03
             PERFORM  RUINYKL1-READ-SEC
             IF    RUINYKL1-INV-FLG  =  SPACE
                   MOVE  WK-END-EDA       TO   RUI-F33
                   REWRITE  RUI-REC
             END-IF
             GO                    TO   MAIN-000
     END-IF.
     GO                            TO   MAIN-000.
 MAIN-010.
*****発注ヘッダを更新する。更新
     IF      MEI-FLG  =  "1"
*************最終枝番をカウントアップ
             ADD      1           TO   WK-END-EDA
             MOVE WK-END-EDA      TO   HAC-F33
             MOVE WK-DATE8        TO   HED-F99
             REWRITE  HED-REC
     END-IF.
     ADD     1                    TO   RUI-CNT.
*
     PERFORM  RCVNYKF-READ-SEC.
*****発注ＨＥＤのみあとで更新する。
     IF ( RUI-F02  NOT =  RCV-F01  AND ERR-KBN = SPACE )
     OR   END-FLG = "END"
**********発注ヘッダ更新
          ADD      1                   TO   WK-END-EDA
          MOVE     WK-END-EDA          TO   HED-F33
          MOVE     WK-DATE8            TO   HED-F99
          REWRITE  HED-REC
          ADD      1                   TO   HED-CNT
     END-IF.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               入庫ファイル項目セット／更新
****************************************************************
 NYKFILL1-SET-SEC            SECTION.
     MOVE "NYKFILL1-SET-SEC"      TO   S-NAME.
*
     MOVE  SPACE                  TO   NYK-REC.
     INITIALIZE                        NYK-REC.
*伝票区分
     MOVE  HED-F01                TO   NYK-F01.
*発注伝票番号
     MOVE  MEI-F02                TO   NYK-F02.
*枝番
     MOVE  WK-END-EDA             TO   NYK-F03.
*相殺区分
     MOVE  ZERO                   TO   NYK-F04.
*行番号
     MOVE  MEI-F03                TO   NYK-F05.
*完了区分
     MOVE  WCM-B12                TO   NYK-F06.
*量販店伝票番号
     MOVE  HED-F09                TO   NYK-F07.
*仕入先コード
     MOVE  HED-F06                TO   NYK-F08.
*取引先コード
     MOVE  HED-F08                TO   NYK-F09.
*税区分
     MOVE  HED-F13                TO   NYK-F10.
*送料区分
     MOVE  HED-F141               TO   NYK-F111.
*送料金額
     MOVE  HED-F142               TO   NYK-F112.
*納入先
     MOVE  HED-F16                TO   NYK-F12.
*発注日
     MOVE  HED-F10                TO   NYK-F13.
*納入予定日
     MOVE  HED-F11                TO   NYK-F14.
*商品コード
     MOVE  MEI-F06                TO   NYK-F15.
*品単
     MOVE  MEI-F07(1:5)           TO   NYK-F16(1:5).
     MOVE  MEI-F07(6:2)           TO   NYK-F16(6:2).
     MOVE  MEI-F07(8:1)           TO   NYK-F16(8:1).
*_番
     IF  WCM-B08 = SPACE
         MOVE  MEI-F08            TO   NYK-F17
     ELSE
         MOVE  WCM-B08            TO   NYK-F17
     END-IF.
*入庫数
     MOVE  WCM-B11                TO   NYK-F18.
*単価区分
     MOVE  SPACE                  TO   NYK-F19.
*仕入単価
     MOVE  MEI-F12                TO   NYK-F20.
*単価区分
     MOVE  MEI-F13                TO   NYK-F21.
*原価単価
     MOVE  MEI-F14                TO   NYK-F22.
*販売単価
     MOVE  MEI-F15                TO   NYK-F23.
*備考
     MOVE  "ｿｳｺｹﾝﾋﾟﾝ"             TO   NYK-F24.
*発注残数
     COMPUTE  NYK-F25  =  MEI-F09  -  MEI-F10.
*実納入日
     MOVE  WCM-B04(1:4)           TO   WK-RUI-F13-1.
     MOVE  WCM-B04(6:2)           TO   WK-RUI-F13-2.
     MOVE  WCM-B04(9:2)           TO   WK-RUI-F13-3.
     MOVE  WK-RUI-F13             TO   NYK-F26.
*倉庫コード
     MOVE  LINK-IN-SOKCD          TO   NYK-F27.
*請求区分
     MOVE  HED-F20                TO   NYK-F28.
*自動発注区分
     MOVE  ZERO                   TO   NYK-F29.
*計上フラグ
     MOVE  ZERO                   TO   NYK-F30.
*計上済フラグ
     MOVE  ZERO                   TO   NYK-F31.
*相手倉庫コード
     MOVE  SPACE                  TO   NYK-F32.
*担当者
     MOVE  WCH-F36                TO   NYK-F33.
*支払締日
     MOVE  WK-ZAIKO-SIME          TO   NYK-F34.
*商品変更区分
*直送請求フラグ
*物流連携区分
*物流連携日
*ユリックス区分
*取消区分
*登録日
     MOVE  WK-DATE8               TO   NYK-F98.
*更新日
     MOVE  WK-DATE8               TO   NYK-F99.
*在庫更新
     PERFORM  ZAIKO-SEC.
*出力
     WRITE  NYK-REC.
     ADD    1                     TO   NYK-CNT.
*
 NYKFILL1-SET-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     CLOSE    HACHEDL1  HACMEIL1  NYKFILL1
              ZAMZAIL1  RUINYKL1
              RCVNYKF   MEIMS1    ZSHIMS1
              ZSOKMS1   JYOKEN1   TANHENL1.
*
     DISPLAY NC"読込　　　件数" " = " RD-CNT   UPON CONS.
     DISPLAY NC"更新エラー件数" " = " ERR-CNT  UPON CONS.
     DISPLAY NC"累積　　　件数" " = " RUI-CNT  UPON CONS.
     DISPLAY NC"社内発注　件数" " = " SKIP-CNT UPON CONS.
     DISPLAY NC"入庫作成　件数" " = " NYK-CNT  UPON CONS.
     DISPLAY NC"発注Ｈ更新件数" " = " HED-CNT  UPON CONS.
     DISPLAY NC"発注Ｍ更新件数" " = " MEI-CNT  UPON CONS.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理
****************************************************************
 ZAIKO-SEC                  SECTION.
*在庫マスタを索引する
     MOVE    LINK-IN-SOKCD  TO   ZAI-F01.
     MOVE    WCM-B06(1:8)   TO   ZAI-F021.
     MOVE    WCM-B06(9:8)   TO   ZAI-F022.
     MOVE    WCM-B08        TO   ZAI-F03.
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
     MOVE    WCM-B06(1:8)   TO   ZAI-F021.
     MOVE    WCM-B06(9:8)   TO   ZAI-F022.
     MOVE    WCM-B08        TO   ZAI-F03.
*****入荷日編集
     MOVE    WCM-F04(1:4)   TO   WK-NYK-YYYY.
     MOVE    WCM-F04(5:2)   TO   WK-NYK-MM.
     MOVE    WCM-F04(7:2)   TO   WK-NYK-DD.
*****日付判定
     IF      WK-ZAIKO-SIME  <  WK-NYK-YMD
*************翌月分
             ADD   WCM-B11  TO   ZAI-F11
     ELSE
*************当月分
             ADD   WCM-B11  TO   ZAI-F07  ZAI-F06
     END-IF.
*商品名称マスタ索引
     MOVE    WCM-B06(1:8)   TO   MES-F011.
     MOVE    WCM-B06(9:5)   TO   MES-F0121.
     MOVE    WCM-B06(14:2)  TO   MES-F0122.
     MOVE    WCM-B06(16:1)  TO   MES-F0123.
     PERFORM MEIMS1-READ-SEC.
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
 ZAIKO-WRITE-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理２
****************************************************************
 ZAIKO-REWRITE-SEC          SECTION.
*
*****入荷日編集
     MOVE    WCM-F04(1:4)   TO   WK-NYK-YYYY.
     MOVE    WCM-F04(5:2)   TO   WK-NYK-MM.
     MOVE    WCM-F04(7:2)   TO   WK-NYK-DD.
*****日付判定
     IF      WK-ZAIKO-SIME  <  WK-NYK-YMD
*************翌月分
             ADD   WCM-B11  TO   ZAI-F11
     ELSE
*************当月分
             ADD   WCM-B11  TO   ZAI-F07  ZAI-F06
     END-IF.
*更新日セット
     MOVE WK-DATE8          TO   ZAI-F99.
*
     REWRITE  ZAI-REC.
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
*      ALL       発注ヘッダファイル読込　　                   *
***************************************************************
 HACHEDL1-READ-SEC      SECTION.
*
     MOVE "HACHEDL1-READ-SEC" TO  S-NAME.
*
     READ    HACHEDL1
       INVALID      KEY
          MOVE     "INV"     TO   HACHEDL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   HACHEDL1-INV-FLG
     END-READ.
*
 HACHEDL1-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       発注明細ファイル読込　　　                   *
***************************************************************
 HACMEIL1-READ-SEC      SECTION.
*
     MOVE "HACMEIL1-READ-SEC" TO  S-NAME.
*
     READ    HACMEIL1
       INVALID      KEY
          MOVE     "INV"     TO   HACMEIL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   HACMEIL1-INV-FLG
     END-READ.
*
 HACMEIL1-READ-EXIT.
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
***************************************************************
*      ALL       倉庫入荷検品確定累積ファイル読込　　　       *
***************************************************************
 RUINYKL1-READ-SEC      SECTION.
*
     MOVE "RUINYKL1-READ-SEC" TO  S-NAME.
*
     READ    RUINYKL1
       INVALID      KEY
          MOVE     "INV"     TO   RUINYKL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   RUINYKL1-INV-FLG
     END-READ.
*
 RUINYKL1-READ-EXIT.
     EXIT.
