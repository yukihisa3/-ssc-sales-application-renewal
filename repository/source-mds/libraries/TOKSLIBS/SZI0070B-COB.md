# SZI0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZI0070B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　在庫ＥＸＣＥＬデータ更新処理　　　*
*    　　　　　　　　　　：　　（作業系）　　　　　　　　　　　*
*    作成日／作成者　　　：　2016/06/28 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、在庫マスタ更新、　　　*
*                            作業実績ファイル作成を行う。　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZI0070B.
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
*作業実績ファイル
     SELECT      SGYFILL1    ASSIGN    TO       DA-01-VI-SGYFILL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SGY-F01
                                                SGY-F02
                             FILE      STATUS   SGY-ST.
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
*在庫ＥＸＣＥＬ取込ファイル
 FD  ZAIWKXX2.
     COPY        ZAIWKXX2    OF        XFDLIB
     JOINING     EXL         AS        PREFIX.
*在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
*作業実績ファイル
 FD  SGYFILL1.
     COPY        SGYFILL1    OF        XFDLIB
     JOINING     SGY         AS        PREFIX.
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
     03  SGY-ST              PIC  X(02)  VALUE  SPACE.
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
     03  SGY-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  RUI-WT-CNT          PIC  9(07)  VALUE  ZERO.
**
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  ZAI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  ZAI-INV-FLG-S           PIC  X(03)  VALUE  SPACE.
 01  ZAI-INV-FLG-N           PIC  X(03)  VALUE  SPACE.
 01  SGY-INV-FLG             PIC  X(03)  VALUE  SPACE.
**
**条件ファイルＲＥＣ保管
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-ZAIKO-SIME     AS  PREFIX.
**
**在庫マスタ（出庫側）ＲＥＣ保管
 COPY     ZAMZAIL1           OF  XFDLIB
 JOINING  SAV-ZAI-S          AS  PREFIX.
**
**在庫マスタ（入庫側）ＲＥＣ保管
 COPY     ZAMZAIL1           OF  XFDLIB
 JOINING  SAV-ZAI-N          AS  PREFIX.
**
**作業実績ファイル（出庫側）ＲＥＣ編集用
 COPY     SGYFILL1           OF  XFDLIB
 JOINING  SGY-S              AS  PREFIX.
**
**作業実績ファイル（入庫側）ＲＥＣ編集用
 COPY     SGYFILL1           OF  XFDLIB
 JOINING  SGY-N              AS  PREFIX.
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
     03  SGY-ERR             PIC  N(10)  VALUE
                   NC"作業実績ファイル異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SZI0070B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SZI0070B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(12)  VALUE
                             NC"在庫ＥＸＣＥＬ読込　　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(12)  VALUE
                             NC"　内．棚移動　　　　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(12)  VALUE
                             NC"　内．ストックＣＨＧ　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(12)  VALUE
                             NC"　内．製品在庫移動　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(12)  VALUE
                             NC"　内．廃棄　　　　　　＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT6.
         05  MSG-OUT6-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT6-FIL2   PIC  N(12)  VALUE
                             NC"累積ファイル作成　　　＝".
         05  MSG-OUT06       PIC  ZZZ,ZZ9.
         05  MSG-OUT6-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT6-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT7.
         05  MSG-OUT7-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT7-FIL2   PIC  N(12)  VALUE
                             NC"在庫マスタ更新　　　　＝".
         05  MSG-OUT07       PIC  ZZZ,ZZ9.
         05  MSG-OUT7-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT7-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT8.
         05  MSG-OUT8-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT8-FIL2   PIC  N(12)  VALUE
                             NC"在庫マスタ作成　　　　＝".
         05  MSG-OUT08       PIC  ZZZ,ZZ9.
         05  MSG-OUT8-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT8-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT9.
         05  MSG-OUT9-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT9-FIL2   PIC  N(12)  VALUE
                             NC"作業実績ファイル作成　＝".
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
 SGY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SGYFILL1.
     DISPLAY     SGY-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SGY-ST      UPON      CONS.
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
     OPEN        I-O         ZAMZAIL1  SGYFILL1.
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
*作業実績ファイル出力・在庫マスタ更新
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
*    作業実績ファイル出力・在庫マスタ更新　　　　              *
****************************************************************
 DATA-UPD-SEC           SECTION.
*
     MOVE       "DATA-UPD-SEC"   TO   S-NAME.
*
*伝票採番
     PERFORM  DPNO-SET-SEC.
*作業実績ファイル項目セット
     PERFORM  SGY-SET-SEC.
*在庫マスタ更新
     PERFORM  ZAI-NEW-SEC.
*作業実績ファイル登録
*  出荷
     WRITE    SGY-REC   FROM  SGY-S-REC.
     ADD      1         TO    SGY-WT-CNT.
*  入荷
     WRITE    SGY-REC   FROM  SGY-N-REC.
     ADD      1         TO    SGY-WT-CNT.
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
* 伝票_を自動採番する（作業_）
*
     CALL   "SKYSNCKB"     USING    OUT-DENNO.
     MOVE   OUT-DENNO               TO   DPNO.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*               作業実績ファイル項目セット                     *
****************************************************************
 SGY-SET-SEC            SECTION.
*
     MOVE  "SGY-SET-SEC"          TO   S-NAME.
*
*在庫マスタ事前取得（棚移動で未出庫FLG=1の場合のみ）
     IF  (  EXL-F052       =           "55" )  AND
         (  EXL-F05H       =           "1"  )
*
*      {出庫側レコード}
*          倉庫ＣＤ(元)
            MOVE  EXL-F054        TO   ZAI-F01
*          商品ＣＤ
            MOVE  EXL-F057        TO   ZAI-F021
*          品単ＣＤ
            MOVE  EXL-F058        TO   ZAI-F022(1:5)
            MOVE  EXL-F059        TO   ZAI-F022(6:2)
            MOVE  EXL-F05A        TO   ZAI-F022(8:1)
*          棚番(出庫)
            MOVE  EXL-F05B        TO   ZAI-F03
*
            PERFORM   ZAI-READ-SEC
            IF        ZAI-INV-FLG      =    "   "
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"倉＝" EXL-F054 " "
*                  NC"商＝" EXL-F057              UPON CONS
*          DISPLAY NC"品＝" EXL-F058 EXL-F059 EXL-F05A
*                  NC"棚＝" EXL-F05B " " "NOT-INVALID" UPON CONS
*↑TEST
                      MOVE  "   "      TO   ZAI-INV-FLG-S
                      MOVE  ZAI-REC    TO   SAV-ZAI-S-REC
            ELSE
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"倉＝" EXL-F054 " "
*                  NC"商＝" EXL-F057              UPON CONS
*          DISPLAY NC"品＝" EXL-F058 EXL-F059 EXL-F05A
*                  NC"棚＝" EXL-F05B " " "INVALID" UPON CONS
*↑TEST
                      INITIALIZE            SAV-ZAI-S-REC
                      MOVE  "INV"      TO   ZAI-INV-FLG-S
                      MOVE  EXL-F054   TO   SAV-ZAI-S-F01
                      MOVE  EXL-F057   TO   SAV-ZAI-S-F021
                      MOVE  EXL-F058   TO   SAV-ZAI-S-F022(1:5)
                      MOVE  EXL-F059   TO   SAV-ZAI-S-F022(6:2)
                      MOVE  EXL-F05A   TO   SAV-ZAI-S-F022(8:1)
                      MOVE  EXL-F05B   TO   SAV-ZAI-S-F03
            END-IF
*
*      {入庫側レコード}
*          倉庫ＣＤ(元)　※作業系は自倉庫のみ＝元
            MOVE  EXL-F054        TO   ZAI-F01
*          商品ＣＤ
            MOVE  EXL-F057        TO   ZAI-F021
*          品単ＣＤ
            MOVE  EXL-F058        TO   ZAI-F022(1:5)
            MOVE  EXL-F059        TO   ZAI-F022(6:2)
            MOVE  EXL-F05A        TO   ZAI-F022(8:1)
*          棚番(入庫)
            MOVE  EXL-F05C        TO   ZAI-F03
*
            PERFORM   ZAI-READ-SEC
            IF        ZAI-INV-FLG      =    "   "
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"倉＝" EXL-F054 " "
*                  NC"商＝" EXL-F057              UPON CONS
*          DISPLAY NC"品＝" EXL-F058 EXL-F059 EXL-F05A
*                  NC"棚＝" EXL-F05C " " "NOT-INVALID" UPON CONS
*↑TEST
                      MOVE  "   "      TO   ZAI-INV-FLG-N
                      MOVE  ZAI-REC    TO   SAV-ZAI-N-REC
            ELSE
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"倉＝" EXL-F054 " "
*                  NC"商＝" EXL-F057              UPON CONS
*          DISPLAY NC"品＝" EXL-F058 EXL-F059 EXL-F05A
*                  NC"棚＝" EXL-F05C " " "INVALID" UPON CONS
*↑TEST
                      INITIALIZE            SAV-ZAI-N-REC
                      MOVE  "INV"      TO   ZAI-INV-FLG-N
                      MOVE  EXL-F054   TO   SAV-ZAI-N-F01
                      MOVE  EXL-F057   TO   SAV-ZAI-N-F021
                      MOVE  EXL-F058   TO   SAV-ZAI-N-F022(1:5)
                      MOVE  EXL-F059   TO   SAV-ZAI-N-F022(6:2)
                      MOVE  EXL-F05A   TO   SAV-ZAI-N-F022(8:1)
                      MOVE  EXL-F05C   TO   SAV-ZAI-N-F03
            END-IF
     ELSE
            CONTINUE
     END-IF.
*
*_出庫ＲＥＣセット　(棚移動・ストック_チェンジとも)
     MOVE   SPACE                 TO   SGY-S-REC.
     INITIALIZE                        SGY-S-REC.
*
*   作業_
     MOVE   DPNO                  TO   SGY-S-F01.
*   行番号
     MOVE   1                     TO   SGY-S-F02.
*   作業区分
     MOVE   EXL-F052              TO   SGY-S-F03.
*   作業場所：倉庫CD(元)
     MOVE   EXL-F054              TO   SGY-S-F04.
*   作業完成日：作業日
     MOVE   EXL-F056              TO   SGY-S-F05.
*   入／出区分
     MOVE   2                     TO   SGY-S-F06.
*   ストック_：ストック_(元)
     MOVE   EXL-F05D              TO   SGY-S-F07.
*   商品ＣＤ
     MOVE   EXL-F057              TO   SGY-S-F08.
*   品単ＣＤ
     MOVE   EXL-F058              TO   SGY-S-F09(1:5).
     MOVE   EXL-F059              TO   SGY-S-F09(6:2).
     MOVE   EXL-F05A              TO   SGY-S-F09(8:1).
*   棚番：出庫棚番
     MOVE   EXL-F05B              TO   SGY-S-F10.
*   数量：作業系_数量
     MOVE   EXL-F05F              TO   SGY-S-F11.
*   備考：作業系_備考
     MOVE   EXL-F05G              TO   SGY-S-F12.
*   計上フラグ F13:INITIAL
*   担当者
     MOVE   LINK-IN-TANCD         TO   SGY-S-F14.
*   未出庫ＦＬＧ
     IF     EXL-F052       =           "55"
            MOVE   EXL-F05H       TO   SGY-S-F15
     END-IF.
*   移動未出庫数（棚移動で未出庫FLG=1の場合のみ）
     IF  (  EXL-F052       =      "55" )  AND
         (  EXL-F05H       =      "1"  )
            IF  EXL-F05F   >      SAV-ZAI-S-F27
                MOVE       SAV-ZAI-S-F27    TO      SGY-S-F16
            ELSE
                MOVE       EXL-F05F         TO      SGY-S-F16
            END-IF
     ELSE
            CONTINUE
     END-IF.
*   移動引当済数（棚移動で未出庫FLG=1の場合のみ）
     IF  (  EXL-F052       =      "55" )  AND
         (  EXL-F05H       =      "1"  )
            IF  EXL-F05F   >      SAV-ZAI-S-F28
                MOVE       SAV-ZAI-S-F28    TO      SGY-S-F17
            ELSE
                MOVE       EXL-F05F         TO      SGY-S-F17
            END-IF
     ELSE
            CONTINUE
     END-IF.
*   予備 F90:INITIAL
*   物流連携ＦＬＧ F93:INITIAL
*   物流連携日 F94:INITIAL
*   部門ＣＤ
     MOVE   EXL-F053              TO   SGY-S-F95
*   ユリックス区分 F96:INITIAL
*   取消ＦＬＧ F97:INITIAL
*   登録日
     MOVE   SYS-DATE8             TO   SGY-S-F98.
*   修正日 F99：INITIAL
*
*_入庫ＲＥＣセット　(棚移動・ストック_チェンジとも)
     MOVE   SPACE                 TO   SGY-N-REC.
     INITIALIZE                        SGY-N-REC.
*
*   作業_
     MOVE   DPNO                  TO   SGY-N-F01.
*   行番号
     MOVE   2                     TO   SGY-N-F02.
*   作業区分
     MOVE   EXL-F052              TO   SGY-N-F03.
*   作業場所：倉庫CD(元) ※作業系は自倉庫のみ＝元
     MOVE   EXL-F054              TO   SGY-N-F04.
*   作業完成日：作業日
     MOVE   EXL-F056              TO   SGY-N-F05.
*   入／出区分
     MOVE   1                     TO   SGY-N-F06.
*   ストック_：ストック_(先)
     MOVE   EXL-F05E              TO   SGY-N-F07.
*   商品ＣＤ
     MOVE   EXL-F057              TO   SGY-N-F08.
*   品単ＣＤ
     MOVE   EXL-F058              TO   SGY-N-F09(1:5).
     MOVE   EXL-F059              TO   SGY-N-F09(6:2).
     MOVE   EXL-F05A              TO   SGY-N-F09(8:1).
*   棚番：入庫棚番
     MOVE   EXL-F05C              TO   SGY-N-F10.
*   数量：作業系_数量
     MOVE   EXL-F05F              TO   SGY-N-F11.
*   備考：作業系_備考
     MOVE   EXL-F05G              TO   SGY-N-F12.
*   計上フラグ F13:INITIAL
*   担当者
     MOVE   LINK-IN-TANCD         TO   SGY-N-F14.
*   未出庫ＦＬＧ
     IF     EXL-F052       =           "55"
            MOVE   EXL-F05H       TO   SGY-N-F15
     END-IF.
*   移動未出庫数（棚移動で未出庫FLG=1の場合のみ）
     IF  (  EXL-F052       =      "55" )  AND
         (  EXL-F05H       =      "1"  )
            IF  EXL-F05F   >      SAV-ZAI-S-F27
                MOVE       SAV-ZAI-S-F27    TO      SGY-N-F16
            ELSE
                MOVE       EXL-F05F         TO      SGY-N-F16
            END-IF
     ELSE
            CONTINUE
     END-IF.
*   移動引当済数（棚移動で未出庫FLG=1の場合のみ）
     IF  (  EXL-F052       =      "55" )  AND
         (  EXL-F05H       =      "1"  )
            IF  EXL-F05F   >      SAV-ZAI-S-F28
                MOVE       SAV-ZAI-S-F28    TO      SGY-N-F17
            ELSE
                MOVE       EXL-F05F         TO      SGY-N-F17
            END-IF
     ELSE
            CONTINUE
     END-IF.
*   予備 F90:INITIAL
*   物流連携ＦＬＧ F93:INITIAL
*   物流連携日 F94:INITIAL
*   部門ＣＤ
     MOVE   EXL-F053              TO   SGY-N-F95
*   ユリックス区分 F96:INITIAL
*   取消ＦＬＧ F97:INITIAL
*   登録日
     MOVE   SYS-DATE8             TO   SGY-N-F98.
*   修正日 F99：INITIAL
*
*
 SGY-SET-EXIT.
     EXIT.
****************************************************************
*                 在庫マスタ更新　　　　　　　　　　　　　　　 *
****************************************************************
 ZAI-NEW-SEC        SECTION.
*
     MOVE "ZAI-NEW-SEC"            TO   S-NAME.
*
 ZAI-NEW-01.
*_出庫情報：55(棚移動)・58(ストックＣＨＧ)とも
     MOVE  2                       TO   ZAI-FLG.
     MOVE  EXL-F054                TO   ZAI-F01.
     MOVE  EXL-F057                TO   ZAI-F021.
     MOVE  EXL-F058                TO   ZAI-F022(1:5).
     MOVE  EXL-F059                TO   ZAI-F022(6:2).
     MOVE  EXL-F05A                TO   ZAI-F022(8:1).
     MOVE  EXL-F05B                TO   ZAI-F03.
     PERFORM     ZAI-READ-SEC.
*
     IF    ZAI-INV-FLG    =  "INV"
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " WRITE"   UPON CONS
*↑TEST
           PERFORM     ZAI-INIT-SEC
           PERFORM     ZAI-CAL-SEC
           MOVE        SYS-DATE8    TO   ZAI-F98
           WRITE       ZAI-REC
           ADD         1            TO   ZAI-WT-CNT
     ELSE
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " REWRITE"   UPON CONS
*↑TEST
           PERFORM     ZAI-CAL-SEC
           MOVE        SYS-DATE8    TO   ZAI-F99
           REWRITE     ZAI-REC
           ADD         1            TO   ZAI-UP-CNT
     END-IF.
*
 ZAI-NEW-02.
*_入庫情報：55(棚移動)・58(ストックＣＨＧ)とも
*
     MOVE  1                       TO   ZAI-FLG.
     MOVE  EXL-F054                TO   ZAI-F01.
     MOVE  EXL-F057                TO   ZAI-F021.
     MOVE  EXL-F058                TO   ZAI-F022(1:5).
     MOVE  EXL-F059                TO   ZAI-F022(6:2).
     MOVE  EXL-F05A                TO   ZAI-F022(8:1).
     MOVE  EXL-F05C                TO   ZAI-F03.
     PERFORM     ZAI-READ-SEC.
*
     IF    ZAI-INV-FLG    =  "INV"
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " WRITE"   UPON CONS
*↑TEST
           PERFORM    ZAI-INIT-SEC
           PERFORM    ZAI-CAL-SEC
           MOVE       SYS-DATE8    TO   ZAI-F98
           WRITE      ZAI-REC
           ADD        1            TO   ZAI-WT-CNT
     ELSE
*↓TEST
*          DISPLAY NC"連＝" EXL-F051 " " NC"作＝" EXL-F052 " "
*                  NC"倉＝" EXL-F054 " REWRITE"   UPON CONS
*↑TEST
           PERFORM    ZAI-CAL-SEC
           MOVE       SYS-DATE8    TO   ZAI-F99
           REWRITE    ZAI-REC
           ADD        1            TO   ZAI-UP-CNT
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
           MOVE  EXL-F054          TO   ZAI-F01
*               商品ＣＤ
           MOVE  EXL-F057          TO   ZAI-F021
*               品単ＣＤ
           MOVE  EXL-F058          TO   ZAI-F022(1:5)
           MOVE  EXL-F059          TO   ZAI-F022(6:2)
           MOVE  EXL-F05A          TO   ZAI-F022(8:1)
*               棚番
           MOVE  EXL-F05C          TO   ZAI-F03
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
           MOVE  EXL-F05B          TO   ZAI-F03
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
 ZAI-CAL-SEC        SECTION.
*
     MOVE "ZAI-CAL-SEC"     TO   S-NAME.
*
     IF    ZAI-FLG     =    2
*出庫レコード
*                  現在庫
           COMPUTE  ZAI-F04  =  ZAI-F04  -  EXL-F05F
           IF       EXL-F056 >  ZAI-SIME
*                  次月出庫
                    COMPUTE  ZAI-F12  =  ZAI-F12  +  EXL-F05F
           ELSE
*                  当月入出庫
                    COMPUTE  ZAI-F06  =  ZAI-F06  -  EXL-F05F
*                  当月出庫
                    COMPUTE  ZAI-F08  =  ZAI-F08  +  EXL-F05F
           END-IF
*         移動未出庫数（棚移動で未出庫FLG=1の場合のみ）
           IF  (  EXL-F052       =       "55" )  AND
               (  EXL-F05H       =       "1"  )
                  IF  EXL-F05F   >       SAV-ZAI-S-F27
                      MOVE       ZERO    TO    ZAI-F27
                  ELSE
                      COMPUTE    ZAI-F27 = ZAI-F27 - EXL-F05F
                  END-IF
           ELSE
                  CONTINUE
           END-IF
*         移動引当済数（棚移動で未出庫FLG=1の場合のみ）
           IF  (  EXL-F052       =       "55" )  AND
               (  EXL-F05H       =       "1"  )
                  IF  EXL-F05F   >       SAV-ZAI-S-F28
                      MOVE       ZERO    TO    ZAI-F28
                  ELSE
                      COMPUTE    ZAI-F28 = ZAI-F28 - EXL-F05F
                  END-IF
           ELSE
                  CONTINUE
           END-IF
     ELSE
*入庫レコード
*                  現在庫
           COMPUTE  ZAI-F04  =  ZAI-F04  +  EXL-F05F
           IF       EXL-F056 >  ZAI-SIME
*                  次月入庫
                    COMPUTE  ZAI-F11  =  ZAI-F11  +  EXL-F05F
           ELSE
*                  当月入出庫
                    COMPUTE  ZAI-F06  =  ZAI-F06  +  EXL-F05F
*                  当月入庫
                    COMPUTE  ZAI-F07  =  ZAI-F07  +  EXL-F05F
           END-IF
*         移動未出庫数（棚移動で未出庫FLG=1の場合のみ）
           IF  (  EXL-F052       =       "55" )  AND
               (  EXL-F05H       =       "1"  )
                  IF  EXL-F05F   >       SAV-ZAI-S-F27
                      COMPUTE    ZAI-F27 = ZAI-F27 + SAV-ZAI-S-F27

                  ELSE
                      COMPUTE    ZAI-F27 = ZAI-F27 + EXL-F05F
                  END-IF
           ELSE
                  CONTINUE
           END-IF
*         移動引当済数（棚移動で未出庫FLG=1の場合のみ）
           IF  (  EXL-F052       =       "55" )  AND
               (  EXL-F05H       =       "1"  )
                  IF  EXL-F05F   >       SAV-ZAI-S-F28
                      COMPUTE    ZAI-F28 = ZAI-F28 + SAV-ZAI-S-F28
                  ELSE
                      COMPUTE    ZAI-F28 = ZAI-F28 + EXL-F05F
                  END-IF
           ELSE
                  CONTINUE
           END-IF
     END-IF.
*
 ZAI-CAL-EXIT.
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
     MOVE     0                 TO        RUI-F11.
* サカタ商品ＣＤ
     MOVE     EXL-F057          TO        RUI-F12.
* 品単１
     MOVE     EXL-F058          TO        RUI-F13.
* 品単２
     MOVE     EXL-F059          TO        RUI-F14.
* 品単３
     MOVE     EXL-F05A          TO        RUI-F15.
* ストック_（元）
     MOVE     EXL-F05D          TO        RUI-F16.
* ストック_（先）
     MOVE     EXL-F05E          TO        RUI-F17.
* 出庫棚番
     MOVE     EXL-F05B          TO        RUI-F18.
* 入庫棚番
     MOVE     EXL-F05C          TO        RUI-F19.
* 数量
     MOVE     EXL-F05F          TO        RUI-F20.
* 未出庫ＦＬＧ
     MOVE     EXL-F05H          TO        RUI-F21.
* 備考
     MOVE     EXL-F05G          TO        RUI-F22.
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
*                 在庫マスタ検索
****************************************************************
 ZAI-READ-SEC                 SECTION.
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
     MOVE      SGY-WT-CNT   TO    MSG-OUT09.
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
               SGYFILL1
               MEIMS1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
