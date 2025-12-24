# SZI0061B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SZI0061B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　在庫ＥＸＣＥＬデータ更新処理　　　*
*    　　　　　　　　　　：　　（入出庫系）　　　　　　　　　　*
*    作成日／作成者　　　：　2016/06/16 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、在庫マスタ更新、　　　*
*                            入庫ファイル作成を行う。　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZI0061B.
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
*在庫ＥＸＣＥＬ取込ファイル
     SELECT      ZAIWKXX2    ASSIGN    TO       DA-01-VI-ZAIWKXX2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      EXL-F052
                                                EXL-F053
                                                EXL-F054
                                                EXL-F055
                                                EXL-F056
                             FILE      STATUS   EXL-ST.
*在庫マスタ
     SELECT      ZAMZAIL1    ASSIGN    TO       DA-01-VI-ZAMZAIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      ZAI-F01
                                                ZAI-F02
                                                ZAI-F03
                             FILE      STATUS   ZAI-ST.
*入出庫ファイル
     SELECT      NYSFILL1    ASSIGN    TO       DA-01-VI-NYSFILL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      NYS-F02
                                                NYS-F03
                             FILE      STATUS   NYS-ST.
*在庫ＥＸＣＥＬ更新累積ファイル
     SELECT      ZAIEXLR1    ASSIGN    TO       DA-01-VI-ZAIEXLR1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      RUI-F03
                                                RUI-F04
                                                RUI-F01
                                                RUI-F10
                                                RUI-F11
                             FILE      STATUS   RUI-ST.
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01 JYO-F02
                             FILE      STATUS   JYO-ST.
*商品名称マスタ
     SELECT      MEIMS1      ASSIGN    TO       DA-01-VI-MEIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F011
                                                MEI-F0121
                                                MEI-F0122
                                                MEI-F0123
                             FILE      STATUS   MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*在庫ＥＸＣＥＬ取込Ｆ
 FD  ZAIWKXX2.
     COPY        ZAIWKXX2    OF        XFDLIB
     JOINING     EXL         AS        PREFIX.
*在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
*入出庫ファイル
 FD  NYSFILL1.
     COPY        NYSFILL1    OF        XFDLIB
     JOINING     NYS         AS        PREFIX.
*在庫ＥＸＣＥＬ更新累積ファイル
 FD  ZAIEXLR1.
     COPY        ZAIEXLR1     OF        XFDLIB
     JOINING     RUI         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  EXL-ST              PIC  X(02)  VALUE  SPACE.
     03  RUI-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  NYS-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-EXL-F052         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F053         PIC  X(04)  VALUE  SPACE.
     03  BR-EXL-F054         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F055         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F056         PIC  9(08)  VALUE  ZERO.
 01  WK-AREA.
     03  DPNO                PIC  9(07)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  ZAI-FLG             PIC  9(01)  VALUE  ZERO.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  EXL-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  CNT-55              PIC  9(07)  VALUE  ZERO.
     03  CNT-58              PIC  9(07)  VALUE  ZERO.
     03  CNT-I3              PIC  9(07)  VALUE  ZERO.
     03  CNT-46              PIC  9(07)  VALUE  ZERO.
     03  ZAI-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  ZAI-UP-CNT          PIC  9(07)  VALUE  ZERO.
     03  NYS-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  RUI-WT-CNT          PIC  9(07)  VALUE  ZERO.
**
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  ZAI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  NYS-INV-FLG             PIC  X(03)  VALUE  SPACE.
**
**条件ファイルＲＥＣ保管
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-ZAIKO-SIME     AS  PREFIX.
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
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  EXL-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ累積Ｆ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  NYS-ERR             PIC  N(10)  VALUE
                   NC"入出庫ファイル異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SZI0061B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SZI0061B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"在庫ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"　内．棚移動　　　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．ストックＣＨＧ＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　内．製品在庫移動　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"　内．廃棄　　　　　＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT6.
         05  MSG-OUT6-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT6-FIL2   PIC  N(11)  VALUE
                             NC"累積ファイル作成　　＝".
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
     03  MSG-OUT9.
         05  MSG-OUT9-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT9-FIL2   PIC  N(11)  VALUE
                             NC"入出庫ファイル作成　＝".
         05  MSG-OUT09       PIC  ZZZ,ZZ9.
         05  MSG-OUT9-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT9-FIL4   PIC  N(01)  VALUE NC"件".
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
 01  LINK-IN-SOKO                PIC  X(02).
 01  LINK-IN-DSOKO               PIC  X(02).
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-SOKO
                                             LINK-IN-DSOKO
                                             LINK-IN-BUMON
                                             LINK-IN-TANCD.
****************************************************************
 DECLARATIVES.
 EXL-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAIWKXX2.
     DISPLAY     EXL-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     EXL-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 NYS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NYSFILL1.
     DISPLAY     NYS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NYS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 RUI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAIEXLR1.
     DISPLAY     RUI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RUI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
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
     OPEN        I-O         ZAIWKXX2.
     OPEN        I-O         ZAIEXLR1.
     OPEN        INPUT       JYOKEN1   MEIMS1.
     OPEN        I-O         ZAMZAIL1  NYSFILL1.
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
*
*条件ファイルＲＥＣ事前取得（経理締め日）
*
*    MOVE     "99"           TO   JYO-F01.
*    MOVE     SPACE          TO   JYO-F02.
*    PERFORM  JYO-READ-SEC.
*    IF       JYO-INV-FLG    =    "INV"
*             DISPLAY
*               NC"＃＃条件ファイル取得エラー（経理締め日）＃＃"
*                                                      UPON CONS
*             MOVE  4001     TO   PROGRAM-STATUS
*             STOP  RUN
*    END-IF.
*
*条件ファイルＲＥＣ事前取得(在庫締日）
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（在庫締め日）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-F05        TO   ZAI-SIME1.
     ADD      1              TO   ZAI-SIME1.
     IF       ZAI-SIME1R2    >    12
              MOVE     1     TO   ZAI-SIME1R2
              ADD      1     TO   ZAI-SIME1R1
     END-IF.
     MOVE     31             TO   ZAI-SIME2.
     MOVE     JYO-F04        TO   WK-ZAIKO-SIME.
*
     MOVE     JYO-REC        TO   JYO-ZAIKO-SIME-REC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    条件ファイル検索
****************************************************************
 JYO-READ-SEC               SECTION.
     MOVE     "JYO-READ-SEC" TO   S-NAME.
*
     READ     JYOKEN1
       INVALID
              MOVE "INV"     TO   JYO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   JYO-INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 在庫ＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-EXL-SEC.
*
     IF       EXL-ENDFLG  =   SPACE
              PERFORM         HENSYU-EXL-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               在庫ＥＸＣＥＬ　順読込
****************************************************************
 READ-EXL-SEC                SECTION.
     MOVE     "READ-EXL-SEC"      TO   S-NAME.
*
     READ     ZAIWKXX2   AT   END
              MOVE      "Y"       TO   EXL-ENDFLG
              GO                  TO   READ-EXL-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   EXL-RD-CNT.
*カウント（作業区分別件数）
     EVALUATE EXL-F052
        WHEN  "55"
              ADD        1        TO   CNT-55
        WHEN  "58"
              ADD        1        TO   CNT-58
        WHEN  "I3"
              ADD        1        TO   CNT-I3
        WHEN  "46"
              ADD        1        TO   CNT-46
*       WHEN  OTHER
*             ADD        1        TO   UNKNOWN
     END-EVALUATE.
*
 READ-EXL-EXIT.
     EXIT.
***************************************************************
*             在庫ＥＸＣＥＬ　チェック編集
***************************************************************
 HENSYU-EXL-SEC             SECTION.
*
     MOVE    "HENSYU-EXL-SEC"      TO   S-NAME.
*
*入出庫ファイル出力・在庫マスタ更新
     PERFORM   DATA-UPD-SEC.
*
*在庫ＥＸＣＥＬ更新累積ファイル出力
     PERFORM   RUI-WRITE-SEC.
*
*在庫ＥＸＣＥＬ取込ファイルＲＥＣ削除
     DELETE    ZAIWKXX2.
*
 HENSYU-EXL-EXIT.
     EXIT.
****************************************************************
*    入出庫ファイル出力・在庫マスタ更新　　　　　              *
****************************************************************
 DATA-UPD-SEC           SECTION.
*
     MOVE       "DATA-UPD-SEC"   TO   S-NAME.
*
*    　　　　作業区分　
     IF    (  EXL-F052  NOT = BR-EXL-F052  ) OR
*    　　　　部門ＣＤ　
           (  EXL-F053  NOT = BR-EXL-F053  ) OR
*    　　　　倉庫ＣＤ（元）
           (  EXL-F054  NOT = BR-EXL-F054  ) OR
*    　　　　倉庫ＣＤ（先）
           (  EXL-F055  NOT = BR-EXL-F055  ) OR
*    　　　　作業日
           (  EXL-F056  NOT = BR-EXL-F056  )
*
*             伝票採番
              PERFORM   DPNO-SET-SEC
*             行リセット
              MOVE      0          TO  I
*             ブレイクキー入替
              MOVE      EXL-F052   TO  BR-EXL-F052
              MOVE      EXL-F053   TO  BR-EXL-F053
              MOVE      EXL-F054   TO  BR-EXL-F054
              MOVE      EXL-F055   TO  BR-EXL-F055
              MOVE      EXL-F056   TO  BR-EXL-F056
     END-IF.
*
*    行
     ADD      1         TO         I.
     IF       I         >          90
*             伝票採番
              PERFORM   DPNO-SET-SEC
*             行リセット
              MOVE      1          TO  I
     END-IF.
*
*入出庫ファイル項目セット
     PERFORM  NYS-SET-SEC.
*在庫マスタ更新
     PERFORM  ZAI-NEW-SEC.
*入出庫ファイル登録
     WRITE    NYS-REC.
     ADD      1         TO   NYS-WT-CNT.
*
 DATA-UPD-EXIT.
     EXIT.
****************************************************************
*                  伝票_　自動採番　　　　　　　　　　　　　　*
****************************************************************
 DPNO-SET-SEC           SECTION.
*
     MOVE   "DPNO-SET-SEC"          TO   S-NAME.
*
* 伝票_を自動採番する
*
     CALL   "SKYNSCKB"     USING    OUT-DENNO.
     MOVE   OUT-DENNO               TO   DPNO.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*                 入出庫ファイル項目セット                     *
****************************************************************
 NYS-SET-SEC            SECTION.
*
     MOVE  "NYS-SET-SEC"          TO   S-NAME.
*
     MOVE   SPACE                 TO   NYS-REC.
     INITIALIZE                        NYS-REC.
*   伝票区分
     MOVE   "32"                  TO   NYS-F01.
*   伝票番号
     MOVE   DPNO                  TO   NYS-F02.
*   行番号
     MOVE   I                     TO   NYS-F03.
*   作業区分
     MOVE   EXL-F052              TO   NYS-F04.
*   商品ＣＤ
     MOVE   EXL-F057              TO   NYS-F05.
*   品単ＣＤ
     MOVE   EXL-F058              TO   NYS-F06(1:5).
     MOVE   EXL-F059              TO   NYS-F06(6:2).
     MOVE   EXL-F05A              TO   NYS-F06(8:1).
*   出庫棚番
     MOVE   EXL-F05I              TO   NYS-F07.
*   入庫棚番
     MOVE   EXL-F05J              TO   NYS-F08.
*   ストック_
     MOVE   EXL-F05K              TO   NYS-F09.
*   出庫場所
     MOVE   EXL-F054              TO   NYS-F10.
*   入庫場所
     MOVE   EXL-F055              TO   NYS-F11.
*   数量
     MOVE   EXL-F05L              TO   NYS-F12.
*   備考
     MOVE   EXL-F05M              TO   NYS-F13.
*   計上フラグ:INITIAL
*   作業日
     MOVE   EXL-F056              TO   NYS-F15.
*   担当者
     MOVE   LINK-IN-TANCD         TO   NYS-F16.
*   出荷場所部門ＣＤ・入荷場所部門ＣＤ
     MOVE   EXL-F053              TO   NYS-F17
                                       NYS-F18.
*   予備:INITIAL
*   出荷物流連携ＦＬＧ:INITIAL
*   出荷物流連携日　　:INITIAL
*   入荷物流連携ＦＬＧ:INITIAL
*   入荷物流連携日　　:INITIAL
*   削除ＦＬＧ　　　　:INITIAL
*   入力倉庫
     MOVE   LINK-IN-SOKO          TO   NYS-F97.
*   登録日
     MOVE   SYS-DATE8             TO   NYS-F98.
*
 NYS-SET-EXIT.
     EXIT.
****************************************************************
*                 在庫マスタ更新　　　　　　　　　　　　　　　 *
****************************************************************
 ZAI-NEW-SEC        SECTION.
*
     MOVE "ZAI-NEW-SEC"            TO   S-NAME.
*
 ZAI-NEW-01.
*_出庫情報：I3(製品在庫移動)・46(廃棄)とも
     IF    EXL-F054    NOT =  SPACE
           MOVE  2                 TO   ZAI-FLG
*         倉庫ＣＤ
           MOVE  EXL-F054          TO   ZAI-F01
*         商品ＣＤ
           MOVE  EXL-F057          TO   ZAI-F021
*         品単ＣＤ
           MOVE  EXL-F058          TO   ZAI-F022(1:5)
           MOVE  EXL-F059          TO   ZAI-F022(6:2)
           MOVE  EXL-F05A          TO   ZAI-F022(8:1)
*         棚番
           MOVE  EXL-F05I          TO   ZAI-F03
           PERFORM     ZAI-READ-SEC
           IF          ZAI-INV-FLG    =  "INV"
                       PERFORM    ZAI-INIT-SEC
                       PERFORM    ZAI-ADD-SEC
                       MOVE       SYS-DATE8    TO   ZAI-F98
                       WRITE      ZAI-REC
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " WRITE"   UPON CONS
*↑TEST
                       ADD        1            TO   ZAI-WT-CNT
           ELSE
                       PERFORM    ZAI-ADD-SEC
                       MOVE       SYS-DATE8    TO   ZAI-F99
                       REWRITE    ZAI-REC
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " REWRITE"   UPON CONS
*↑TEST
                       ADD        1            TO   ZAI-UP-CNT
           END-IF
     END-IF.
*
 ZAI-NEW-02.
*_入庫情報：I3(製品在庫移動)のみ
*
     IF    EXL-F052    NOT =  "I3"
           GO                      TO   ZAI-NEW-EXIT
     END-IF.
*
     IF    EXL-F055    NOT =  SPACE
           MOVE  1                 TO   ZAI-FLG
*         倉庫ＣＤ
           MOVE  EXL-F055          TO   ZAI-F01
*         商品ＣＤ
           MOVE  EXL-F057          TO   ZAI-F021
*         品単ＣＤ
           MOVE  EXL-F058          TO   ZAI-F022(1:5)
           MOVE  EXL-F059          TO   ZAI-F022(6:2)
           MOVE  EXL-F05A          TO   ZAI-F022(8:1)
*         棚番
           MOVE  EXL-F05J          TO   ZAI-F03
           PERFORM     ZAI-READ-SEC
           IF          ZAI-INV-FLG    =  "INV"
                       PERFORM    ZAI-INIT-SEC
                       PERFORM    ZAI-ADD-SEC
                       MOVE       SYS-DATE8    TO   ZAI-F98
                       WRITE      ZAI-REC
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F055 " WRITE"   UPON CONS
*↑TEST
                       ADD        1            TO   ZAI-WT-CNT
           ELSE
                       PERFORM    ZAI-ADD-SEC
                       MOVE       SYS-DATE8    TO   ZAI-F99
                       REWRITE    ZAI-REC
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F055 " REWRITE"   UPON CONS
*↑TEST
                       ADD        1            TO   ZAI-UP-CNT
           END-IF
     END-IF.
*
 ZAI-NEW-EXIT.
     EXIT.
****************************************************************
*                在庫マスタ　初期項目編集　　　　　　　　　　　*
****************************************************************
 ZAI-INIT-SEC       SECTION.
*
     MOVE "ZAI-INIT-SEC"      TO   S-NAME.
*
     MOVE  SPACE              TO   ZAI-REC.
     INITIALIZE                    ZAI-REC.
*
     IF    ZAI-FLG     =    1
*          入庫
*               倉庫ＣＤ
           MOVE  EXL-F055          TO   ZAI-F01
*               商品ＣＤ
           MOVE  EXL-F057          TO   ZAI-F021
*               品単ＣＤ
           MOVE  EXL-F058          TO   ZAI-F022(1:5)
           MOVE  EXL-F059          TO   ZAI-F022(6:2)
           MOVE  EXL-F05A          TO   ZAI-F022(8:1)
*               棚番
           MOVE  EXL-F05J          TO   ZAI-F03
     ELSE
*          出庫
*               倉庫ＣＤ
           MOVE  EXL-F054          TO   ZAI-F01
*               商品ＣＤ
           MOVE  EXL-F057          TO   ZAI-F021
*               品単ＣＤ
           MOVE  EXL-F058          TO   ZAI-F022(1:5)
           MOVE  EXL-F059          TO   ZAI-F022(6:2)
           MOVE  EXL-F05A          TO   ZAI-F022(8:1)
*               棚番
           MOVE  EXL-F05I          TO   ZAI-F03
     END-IF.
*---<  商品名称（カナ）取得  >---*
*            商品ＣＤ
     MOVE     ZAI-F021        TO      MEI-F011.
*            品単ＣＤ
     MOVE     ZAI-F022        TO      MEI-F012.
     PERFORM  MEI-READ-SEC.
     MOVE     MEI-F031        TO      ZAI-F30.
*
 ZAI-INIT-EXIT.
     EXIT.
****************************************************************
*                 　在庫計算
****************************************************************
 ZAI-ADD-SEC        SECTION.
*
     MOVE "ZAI-ADD-SEC"     TO   S-NAME.
*
     IF    ZAI-FLG     =    2
*出庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  -  EXL-F05L
           IF       EXL-F056 >  ZAI-SIME
                    COMPUTE  ZAI-F12  =  ZAI-F12  +  EXL-F05L
           ELSE
                    COMPUTE  ZAI-F06  =  ZAI-F06  -  EXL-F05L
                    COMPUTE  ZAI-F08  =  ZAI-F08  +  EXL-F05L
           END-IF
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  +  EXL-F05L
           IF       EXL-F056 >  ZAI-SIME
                    COMPUTE  ZAI-F11  =  ZAI-F11  +  EXL-F05L
           ELSE
                    COMPUTE  ZAI-F06  =  ZAI-F06  +  EXL-F05L
                    COMPUTE  ZAI-F07  =  ZAI-F07  +  EXL-F05L
           END-IF
     END-IF.
*
 ZAI-ADD-EXIT.
     EXIT.
****************************************************************
*       在庫ＥＸＣＥＬ更新累積ファイル出力
****************************************************************
 RUI-WRITE-SEC               SECTION.
*
     MOVE    "RUI-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        RUI-REC.
     INITIALIZE                           RUI-REC.
*
* 作成日付
     MOVE     SYS-DATE8         TO        RUI-F01.
* 作成時刻
     MOVE     WK-TIME-HM        TO        RUI-F02.
* 作成担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        RUI-F03.
* 作成担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        RUI-F04.
* 作業区分
     MOVE     EXL-F052          TO        RUI-F05.
* 部門ＣＤ
     MOVE     EXL-F053          TO        RUI-F06.
* 倉庫ＣＤ（元）
     MOVE     EXL-F054          TO        RUI-F07.
* 倉庫ＣＤ（先）
     MOVE     EXL-F055          TO        RUI-F08.
* 作業日
     MOVE     EXL-F056          TO        RUI-F09.
* 伝票番号
     MOVE     DPNO              TO        RUI-F10.
* 行番号
     MOVE     I                 TO        RUI-F11.
* サカタ商品ＣＤ
     MOVE     EXL-F057          TO        RUI-F12.
* 品単１
     MOVE     EXL-F058          TO        RUI-F13.
* 品単２
     MOVE     EXL-F059          TO        RUI-F14.
* 品単３
     MOVE     EXL-F05A          TO        RUI-F15.
* ストック_（元）
     MOVE     EXL-F05K          TO        RUI-F16.
* ストック_（先）:INITIAL
* 出庫棚番
     MOVE     EXL-F05I          TO        RUI-F18.
* 入庫棚番
     MOVE     EXL-F05J          TO        RUI-F19.
* 数量
     MOVE     EXL-F05L          TO        RUI-F20.
* 未出庫ＦＬＧ:INITIAL
* 備考
     MOVE     EXL-F05M          TO        RUI-F22.
* 予備：INITIAL
* 取込日付
     MOVE     EXL-F01           TO        RUI-F96.
* 取込時刻
     MOVE     EXL-F02           TO        RUI-F97.
* 取込担当者部門ＣＤ
     MOVE     EXL-F03           TO        RUI-F98.
* 取込担当者ＣＤ
     MOVE     EXL-F04           TO        RUI-F99.
*
     WRITE    RUI-REC.
     ADD      1                 TO        RUI-WT-CNT.
*
 RUI-WRITE-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ検索
****************************************************************
 MEI-READ-SEC             SECTION.
*
     MOVE     "MEI-READ-SEC"    TO    S-NAME.
*
     READ      MEIMS1
          INVALID
               MOVE   "INV"     TO    MEI-INV-FLG
          NOT INVALID
               MOVE   "   "     TO    MEI-INV-FLG
     END-READ.
*
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*               在庫マスタ検索
****************************************************************
 ZAI-READ-SEC               SECTION.
*
     MOVE     "ZAI-READ-SEC"      TO   S-NAME.
*
     READ      ZAMZAIL1
       INVALID
               MOVE    "INV"      TO   ZAI-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   ZAI-INV-FLG
     END-READ.
*
 ZAI-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      EXL-RD-CNT   TO    MSG-OUT01.
     MOVE      CNT-55       TO    MSG-OUT02.
     MOVE      CNT-58       TO    MSG-OUT03.
     MOVE      CNT-I3       TO    MSG-OUT04.
     MOVE      CNT-46       TO    MSG-OUT05.
     MOVE      RUI-WT-CNT   TO    MSG-OUT06.
     MOVE      ZAI-UP-CNT   TO    MSG-OUT07.
     MOVE      ZAI-WT-CNT   TO    MSG-OUT08.
     MOVE      NYS-WT-CNT   TO    MSG-OUT09.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
     DISPLAY   MSG-OUT5-FIL1 MSG-OUT5-FIL2 MSG-OUT05
               MSG-OUT5-FIL3 MSG-OUT5-FIL4 UPON CONS.
     DISPLAY   MSG-OUT7-FIL1 MSG-OUT7-FIL2 MSG-OUT07
               MSG-OUT7-FIL3 MSG-OUT7-FIL4 UPON CONS.
     DISPLAY   MSG-OUT8-FIL1 MSG-OUT8-FIL2 MSG-OUT08
               MSG-OUT8-FIL3 MSG-OUT8-FIL4 UPON CONS.

     DISPLAY   MSG-OUT6-FIL1 MSG-OUT6-FIL2 MSG-OUT06
               MSG-OUT6-FIL3 MSG-OUT6-FIL4 UPON CONS.
     DISPLAY   MSG-OUT9-FIL1 MSG-OUT9-FIL2 MSG-OUT09
               MSG-OUT9-FIL3 MSG-OUT9-FIL4 UPON CONS.
*
     CLOSE     ZAIWKXX2
               ZAIEXLR1
               JYOKEN1
               ZAMZAIL1
               NYSFILL1
               MEIMS1.
*
*ＯＵＴパラメタセット
* 取込件数
*    MOVE      EXL-RD-CNT      TO    LINK-OUT-CNT1.
* 棚移動件数
*    MOVE      CNT-55       TO    LINK-OUT-CNT2.
* ストック_ＣＨＧ件数
*    MOVE      CNT-58       TO    LINK-OUT-CNT3.
* 製品在庫移動件数
*    MOVE      CNT-I3       TO    LINK-OUT-CNT4.
* 廃棄件数
*    MOVE      CNT-46       TO    LINK-OUT-CNT5.
* エラー件数
*    MOVE      ERR-CNT      TO    LINK-OUT-CNT6.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
