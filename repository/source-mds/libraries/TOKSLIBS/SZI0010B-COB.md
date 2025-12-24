# SZI0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZI0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　在庫ＥＸＣＥＬデータチェック　　　*
*    　　　　　　　　　　：　　（入出庫系）　　　　　　　　　　*
*    作成日／作成者　　　：　2016/06/06 INOUE                  *
*    処理内容　　　　　　：　在庫ＥＸＣＥＬデータについて、　　*
*    　　　　　　　　　　　　整合性チェック・件数カウントを　　*
*                            を行う。（入出庫系）　　　　　　　*
*    変更日／作成者　　　：　2016/06/09 INOUE                  *
*    変更内容　　　　　　：　_作業日チェックをカット（不要）　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZI0010B.
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
*在庫ＥＸＣＥＬデータ（入出庫系）※ＣＳＶ形式
     SELECT      ZAINSKXX    ASSIGN    TO       DA-01-S-ZAINSKXX
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*在庫ＥＸＣＥＬ取込ファイル
     SELECT      ZAIWKXX     ASSIGN    TO       DA-01-VS-ZAIWKXX
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   ZAI-ST.
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
*倉庫マスタ
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*在庫ＥＸＣＥＬデータ（入出庫系）
 FD  ZAINSKXX
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    46        RECORDS.
     COPY        ZAINSKXX    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*在庫ＥＸＣＥＬ取込ファイル
 FD  ZAIWKXX.
     COPY        ZAIWKXX     OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  ZAI-CNT             PIC  9(07)  VALUE  ZERO.
     03  CNT-55              PIC  9(07)  VALUE  ZERO.
     03  CNT-58              PIC  9(07)  VALUE  ZERO.
     03  CNT-I3              PIC  9(07)  VALUE  ZERO.
     03  CNT-46              PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  10.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-CSV-F03              PIC  X(04)  VALUE SPACE.
 01  WK-CSV-F03-X            REDEFINES   WK-CSV-F03.
     03  WK-CSV-F03R         PIC  9(04).
*
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
**
**条件ファイルＲＥＣ保管
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-KEIRI-SIME     AS  PREFIX.
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
*経理〆日
 01  WK-SIME                 PIC  9(08).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  CSV-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬデータ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SZI0010B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SZI0010B".
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
                             NC"取込ファイル作成　　＝".
         05  MSG-OUT06       PIC  ZZZ,ZZ9.
         05  MSG-OUT6-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT6-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT7.
         05  MSG-OUT7-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT7-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT07       PIC  ZZZ,ZZ9.
         05  MSG-OUT7-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT7-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT8.
         05  MSG-OUT8-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT8-FIL2   PIC  N(11)  VALUE
                             NC"　内．作業区分不明　＝".
         05  MSG-OUT08       PIC  ZZZ,ZZ9.
         05  MSG-OUT8-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT8-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT9.
         05  MSG-OUT9-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT9-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
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
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-SOKO                PIC  X(02).
 01  LINK-IN-DSOKO               PIC  X(02).
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
 01  LINK-OUT-CNT3               PIC  9(07).
 01  LINK-OUT-CNT4               PIC  9(07).
 01  LINK-OUT-CNT5               PIC  9(07).
 01  LINK-OUT-CNT6               PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-SOKO
                                             LINK-IN-DSOKO
                                             LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-OUT-CNT1
                                             LINK-OUT-CNT2
                                             LINK-OUT-CNT3
                                             LINK-OUT-CNT4
                                             LINK-OUT-CNT5
                                             LINK-OUT-CNT6.
****************************************************************
 DECLARATIVES.
 CSV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAINSKXX.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAIWKXX.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
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
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
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
     OPEN        INPUT       ZAINSKXX.
     OPEN        OUTPUT      ZAIWKXX.
     OPEN        INPUT       JYOKEN1   ZSOKMS1  MEIMS1.
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
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*
*条件ファイルＲＥＣ事前取得（経理締め日）
*
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（経理締め日）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-REC        TO   JYO-KEIRI-SIME-REC.
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
     PERFORM  READ-CSV-SEC.
*
     IF       CSV-ENDFLG  =   SPACE
              PERFORM         HENSYU-CSV-SEC
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
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     ZAINSKXX   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   CSV-CNT.
*カウント（作業区分別件数）
     EVALUATE CSV-F02
        WHEN  "55"
              ADD        1        TO   CNT-55
        WHEN  "58"
              ADD        1        TO   CNT-58
        WHEN  "I3"
              ADD        1        TO   CNT-I3
        WHEN  "46"
              ADD        1        TO   CNT-46
        WHEN  OTHER
              ADD        1        TO   UNKNOWN
     END-EVALUATE.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*             在庫ＥＸＣＥＬ　チェック編集
***************************************************************
 HENSYU-CSV-SEC             SECTION.
*
     MOVE    "HENSYU-CSV-SEC"      TO   S-NAME.
*
     MOVE     SPACE                TO   WK-ERR-KBN.
     INITIALIZE                         WK-ERR-TBL.
*
*データ項目チェック
     PERFORM   DATA-CHK-SEC.
*
*在庫ＥＸＣＥＬ取込ファイル出力
     PERFORM   ZAI-WRITE-SEC.
*
 HENSYU-CSV-EXIT.
     EXIT.
***************************************************************
*             データ項目チェック
***************************************************************
 DATA-CHK-SEC     SECTION.
*
     MOVE   "DATA-CHK-SEC"       TO   S-NAME.
*
*T   DISPLAY NC"連番＝" CSV-F01  UPON CONS.
*
*------------------------------------------*
 DATA-CHK-00.
*【必須項目チェック】
*------------------------------------------*
*  『連番』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  ( CSV-F01 < 1         ) OR
         ( CSV-F01 NOT NUMERIC )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"連番エラー　" CSV-F01  UPON CONS
     END-IF.
*
*  『作業区分』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  CSV-F02 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"作業区分エラー　" CSV-F02  UPON CONS
     END-IF.
*
*  『部門ＣＤ』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  CSV-F03 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"部門ＣＤエラー　" CSV-F03  UPON CONS
     END-IF.
*
*  『倉庫ＣＤ（元）』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  CSV-F04 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"倉庫ＣＤ（元）エラー　" CSV-F04  UPON CONS
     END-IF.
*
*  『倉庫ＣＤ（先）』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：不要
     IF  CSV-F02 = "I3"
         IF  CSV-F05 =   SPACE
             MOVE   "Y"          TO        WK-ERR-KBN
             MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"倉庫ＣＤ（先）エラー　" CSV-F05  UPON CONS
         END-IF
     END-IF.
*
*  『作業日』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  ( CSV-F06 = ZERO      ) OR
         ( CSV-F06 NOT NUMERIC )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"作業日エラー　"  CSV-F06  UPON CONS
     END-IF.
*
*  『サカタ商品ＣＤ』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  CSV-F07 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"サカタ商品ＣＤエラー　" CSV-F07  UPON CONS
     END-IF.
*
*  『サカタ品単１』
*      I3(製品在庫移動)：必須だが””もＯＫ
*      46(廃棄)　　　　：必須だが””もＯＫ
*T       DISPLAY NC"サカタ品単１　" CSV-F08  UPON CONS
*
*  『サカタ品単２』
*      I3(製品在庫移動)：必須だが””もＯＫ
*      46(廃棄)　　　　：必須だが””もＯＫ
*T       DISPLAY NC"サカタ品単２　" CSV-F09  UPON CONS
*
*  『サカタ品単３』
*      I3(製品在庫移動)：必須だが””もＯＫ
*      46(廃棄)　　　　：必須だが””もＯＫ
*T       DISPLAY NC"サカタ品単３　" CSV-F10  UPON CONS
*
*  『出庫棚番』
*      I3(製品在庫移動)：必須だが””もＯＫ
*      46(廃棄)　　　　：必須だが””もＯＫ
*
*  『入庫棚番』
*      I3(製品在庫移動)：必須だが””もＯＫ
*      46(廃棄)　　　　：不要
*
*  『ストック_』
*      I3(製品在庫移動)：任意
*      46(廃棄)　　　　：任意
*
*  『数量』
*      I3(製品在庫移動)：必須
*      46(廃棄)　　　　：必須
     IF  CSV-F14 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
*T       DISPLAY NC"数量エラー　"  CSV-F14  UPON CONS
     END-IF.
*
*  『備考』
*      I3(製品在庫移動)：任意
*      46(廃棄)　　　　：任意
*
*------------------------------------------*
 DATA-CHK-01.
*【作業区分チェック】
*------------------------------------------*
     IF    (  CSV-F02   NOT  =   "46" ) AND
           (  CSV-F02   NOT  =   "I3" )
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(01)
*T            DISPLAY NC"作業区分不一致　" CSV-F02  UPON CONS
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【倉庫チェック】　作業区分＝46(廃棄)の場合
*------------------------------------------*
     IF       CSV-F02        NOT  =       "46"
              GO             TO   DATA-CHK-03
     END-IF.
*
* 『倉庫ＣＤ（元）』
*   _倉庫マスタ存在チェック
     MOVE     CSV-F04        TO   SOK-F01.
     PERFORM  SOKO-READ-SEC.
     IF       SOK-INV-FLG =  "INV"
              MOVE    "Y"    TO   WK-ERR-KBN
              MOVE    "1"    TO   ERR-KBN(02)
*T            DISPLAY NC"倉庫マスタなし"  CSV-F04  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*   _取扱部門ＣＤ一致チェック
*
     MOVE     "20"           TO   JYO-F01.
     MOVE     CSV-F04        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY NC"条件ファイルなし！　"  "F01=20 "
                                                "F02=" CSV-F04
                                                 UPON CONS
              MOVE   4001    TO   PROGRAM-STATUS
              STOP   RUN
     END-IF.
*
     MOVE     CSV-F03      TO        WK-CSV-F03.
     IF     ( WK-CSV-F03R  =   JYO-F08 ) OR
            ( WK-CSV-F03R  =   JYO-F09 ) OR
            ( WK-CSV-F03R  =   JYO-F10 ) OR
            ( WK-CSV-F03R  =   JYO-F11 ) OR
            ( WK-CSV-F03R  =   JYO-F12 )
              CONTINUE
     ELSE
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T            DISPLAY NC"取扱部門不一致　" CSV-F03  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F08  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F09  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F10  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F11  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F12  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*   _倉庫ＣＤ制限チェック
     IF       LINK-IN-DSOKO  =    "99"
              IF   CSV-F04   =    LINK-IN-SOKO
                   CONTINUE
              ELSE
*T        DISPLAY NC"許可倉庫ＣＤ以外　" CSV-F04  UPON CONS
*T        DISPLAY NC"　　　　代表倉庫＝" LINK-IN-DSOKO UPON CONS
                   MOVE     "Y"   TO   WK-ERR-KBN
                   MOVE     "1"   TO   ERR-KBN(02)
                   GO             TO   DATA-CHK-04
              END-IF
     END-IF.
*
*--------------------------------------------------*
 DATA-CHK-03.
*【倉庫チェック】　作業区分＝I3(製品在庫移動)の場合
*--------------------------------------------------*
     IF       CSV-F02        NOT  =       "I3"
              GO             TO   DATA-CHK-04
     END-IF.
*
* 『倉庫ＣＤ（元）』
*   _倉庫マスタ存在チェック
     MOVE     CSV-F04        TO   SOK-F01.
     PERFORM  SOKO-READ-SEC.
     IF       SOK-INV-FLG =  "INV"
              MOVE    "Y"    TO   WK-ERR-KBN
              MOVE    "1"    TO   ERR-KBN(02)
*T            DISPLAY NC"倉庫マスタなし"  CSV-F04  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*   _取扱部門ＣＤ一致チェック
*
     MOVE     "20"           TO   JYO-F01.
     MOVE     CSV-F04        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY NC"条件ファイルなし！　"  "F01=20 "
                                                "F02=" CSV-F04
                                                 UPON CONS
              MOVE   4001    TO   PROGRAM-STATUS
              STOP   RUN
     END-IF.
*
     MOVE     CSV-F03      TO        WK-CSV-F03.
     IF     ( WK-CSV-F03R  =   JYO-F08 ) OR
            ( WK-CSV-F03R  =   JYO-F09 ) OR
            ( WK-CSV-F03R  =   JYO-F10 ) OR
            ( WK-CSV-F03R  =   JYO-F11 ) OR
            ( WK-CSV-F03R  =   JYO-F12 )
              CONTINUE
     ELSE
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T            DISPLAY NC"取扱部門不一致　" CSV-F03  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F08  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F09  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F10  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F11  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F12  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*   _倉庫ＣＤ制限チェック
     IF       LINK-IN-DSOKO  =    "99"
              IF   CSV-F04   =    LINK-IN-SOKO
                   CONTINUE
              ELSE
*T        DISPLAY NC"許可倉庫ＣＤ以外　" CSV-F04  UPON CONS
*T        DISPLAY NC"　　　　代表倉庫＝" LINK-IN-DSOKO UPON CONS
                   MOVE     "Y"   TO   WK-ERR-KBN
                   MOVE     "1"   TO   ERR-KBN(02)
                   GO             TO   DATA-CHK-04
              END-IF
     END-IF.
*
*
* 『倉庫ＣＤ（先）』
*   _倉庫マスタ存在チェック
     MOVE     CSV-F05        TO   SOK-F01.
     PERFORM  SOKO-READ-SEC.
     IF       SOK-INV-FLG =  "INV"
              MOVE    "Y"    TO   WK-ERR-KBN
              MOVE    "1"    TO   ERR-KBN(02)
*T            DISPLAY NC"倉庫マスタなし"  CSV-F05  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*   _取扱部門ＣＤ一致チェック
*
     MOVE     "20"           TO   JYO-F01.
     MOVE     CSV-F05        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY NC"条件ファイルなし！　"  "F01=20 "
                                                "F02=" CSV-F05
                                                 UPON CONS
              MOVE   4001    TO   PROGRAM-STATUS
              STOP   RUN
     END-IF.
*
     MOVE     CSV-F03      TO        WK-CSV-F03.
     IF     ( WK-CSV-F03R  =   JYO-F08 ) OR
            ( WK-CSV-F03R  =   JYO-F09 ) OR
            ( WK-CSV-F03R  =   JYO-F10 ) OR
            ( WK-CSV-F03R  =   JYO-F11 ) OR
            ( WK-CSV-F03R  =   JYO-F12 )
              CONTINUE
     ELSE
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T            DISPLAY NC"取扱部門不一致　" CSV-F03  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F08  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F09  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F10  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F11  UPON CONS
*T            DISPLAY NC"　　条件Ｆ部門＝" JYO-F12  UPON CONS
              GO             TO   DATA-CHK-04
     END-IF.
*
*------------------------------------------*
 DATA-CHK-04.
*【商品ＣＤチェック】
*------------------------------------------*
     MOVE     CSV-F07        TO     MEI-F011.
     MOVE     CSV-F08        TO     MEI-F0121.
     MOVE     CSV-F09        TO     MEI-F0122.
     MOVE     CSV-F10        TO     MEI-F0123.
     PERFORM  MEI-READ-SEC.
     IF       MEI-INV-FLG =  "INV"
              MOVE    "Y"    TO   WK-ERR-KBN
              MOVE    "1"    TO   ERR-KBN(04)
*T            DISPLAY NC"商品名称マスタなし　"  CSV-F07 "-"
*T                                              CSV-F08 "-"
*T                                              CSV-F09 "-"
*T                                              CSV-F10
*T                                              UPON CONS
     END-IF.
*
*------------------------------------------*
 DATA-CHK-05.
*【数量チェック】
*------------------------------------------*
     IF  CSV-F14 < 1
     OR  CSV-F14 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(05)
*T               DISPLAY NC"金額エラー"  CSV-F14  UPON CONS
     END-IF.
*
*------------------------------------------*
 DATA-CHK-06.
*【作業日チェック】
*------------------------------------------*
* _論理チェック
     MOVE    "2"               TO      LINK-IN-KBN.
     MOVE     ZERO             TO      LINK-IN-YMD6.
     MOVE     CSV-F06          TO      LINK-IN-YMD8.
     MOVE     ZERO             TO      LINK-OUT-RET.
     MOVE     ZERO             TO      LINK-OUT-YMD.
     CALL    "SKYDTCKB"        USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET     NOT =   ZERO
*T            DISPLAY NC"作業日論理エラー"  CSV-F06 UPON CONS
              MOVE    "Y"      TO      WK-ERR-KBN
              MOVE    "1"      TO      ERR-KBN(06)
              GO               TO      DATA-CHK-07
     END-IF.
*
* _〆日との関連チェック
*   入力した作業日の対象となる締日をワークにセットする
     EVALUATE   CSV-F06(5:2)
         WHEN   01    MOVE  JYO-KEIRI-SIME-F04   TO   WK-SIME
         WHEN   02    MOVE  JYO-KEIRI-SIME-F05   TO   WK-SIME
         WHEN   03    MOVE  JYO-KEIRI-SIME-F06   TO   WK-SIME
         WHEN   04    MOVE  JYO-KEIRI-SIME-F07   TO   WK-SIME
         WHEN   05    MOVE  JYO-KEIRI-SIME-F08   TO   WK-SIME
         WHEN   06    MOVE  JYO-KEIRI-SIME-F09   TO   WK-SIME
         WHEN   07    MOVE  JYO-KEIRI-SIME-F10   TO   WK-SIME
         WHEN   08    MOVE  JYO-KEIRI-SIME-F11   TO   WK-SIME
         WHEN   09    MOVE  JYO-KEIRI-SIME-F12   TO   WK-SIME
         WHEN   10    MOVE  JYO-KEIRI-SIME-F12A  TO   WK-SIME
         WHEN   11    MOVE  JYO-KEIRI-SIME-F12B  TO   WK-SIME
         WHEN   12    MOVE  JYO-KEIRI-SIME-F12C  TO   WK-SIME
     END-EVALUATE.
*   締日－１算出
     COMPUTE    WK-SIME  =  WK-SIME  -  1.
*   システム日付≦（〆日-1日）はＯＫ
     IF  SYS-DATE8     <=  WK-SIME
         CONTINUE
     ELSE
         MOVE   "Y"        TO   WK-ERR-KBN
         MOVE   "1"        TO   ERR-KBN(06)
*T      DISPLAY NC"システム日付≦（〆日－１日）エラー" UPON CONS
*T      DISPLAY NC"　　　　　　　　　　システム日付＝" SYS-DATE8
*T                                                     UPON CONS
*T      DISPLAY NC"　　　　　　　　　　　〆日－１日＝" WK-SIME
*T                                                     UPON CONS
         GO                TO   DATA-CHK-07
     END-IF.
*
* _作業日≦システム日付はＯＫ
     IF  CSV-F06       <=  SYS-DATE8
         CONTINUE
     ELSE
         MOVE   "Y"        TO   WK-ERR-KBN
         MOVE   "1"        TO   ERR-KBN(06)
*T      DISPLAY NC"作業日＞システム日付エラー" UPON CONS
*T      DISPLAY NC"　　　　　　　　　作業日＝" CSV-F06
*T                                                     UPON CONS
*T      DISPLAY NC"　　　　　　システム日付＝" SYS-DATE8
*T                                                     UPON CONS
         GO                TO   DATA-CHK-07
     END-IF.
*
* _作業月１日～システム日付はＯＫ  20160609廃止
*    MOVE     CSV-F06          TO      WK-SAGYOUBI-1.
*    MOVE     01               TO      WK-SAGYOUBI-D.
*T   DISPLAY NC"作業月１日＝"  WK-SAGYOUBI-1  UPON CONS.
*    IF    (  WK-SAGYOUBI-1    <=      CSV-F06   ) AND
*          (  CSV-F06          <=      SYS-DATE8 )
*             CONTINUE
*    ELSE
*             MOVE   "Y"       TO      WK-ERR-KBN
*             MOVE   "1"       TO      ERR-KBN(06)
*T      DISPLAY NC"作業月１日～システム日付エラー" UPON CONS
*T      DISPLAY NC"　　　　　　　作業月１日＝" WK-SAGYOUBI-1
*T      DISPLAY NC"　　　　　　　　　作業日＝" CSV-F06
*T                                                     UPON CONS
*T      DISPLAY NC"　　　　　　システム日付＝" SYS-DATE8
*T                                                     UPON CONS
*             GO               TO       DATA-CHK-07
*    END-IF.
*
*------------------------------------------*
 DATA-CHK-07.
*【部門チェック】
*------------------------------------------*
     MOVE     "22"           TO   JYO-F01.
     MOVE     CSV-F03        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(07)
*T            DISPLAY NC"条件ファイルなし！　"  "F01=22 "
*T                                              "F02=" CSV-F03
*T                                               UPON CONS
     END-IF.
*
*------------------------------------------*
 DATA-CHK-08.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     ERR-CNT
     ELSE
              ADD    1        TO     OK-CNT
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*       在庫ＥＸＣＥＬ取込ファイル出力
****************************************************************
 ZAI-WRITE-SEC               SECTION.
*
     MOVE       "ZAI-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        ZAI-REC.
     INITIALIZE                           ZAI-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        ZAI-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        ZAI-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        ZAI-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        ZAI-F04.
* 連番
     MOVE     CSV-F01           TO        ZAI-F051.
* 作業区分
     MOVE     CSV-F02           TO        ZAI-F052.
* 部門ＣＤ
     MOVE     CSV-F03           TO        ZAI-F053.
* 倉庫ＣＤ（元）
     MOVE     CSV-F04           TO        ZAI-F054.
* 倉庫ＣＤ（先）　製品在庫移動のみ
     IF       CSV-F02           =         "I3"
              MOVE   CSV-F05    TO        ZAI-F055
     END-IF.
* 作業日
     MOVE     CSV-F06           TO        ZAI-F056.
* サカタ商品ＣＤ
     MOVE     CSV-F07           TO        ZAI-F057.
* 品単１
     MOVE     CSV-F08           TO        ZAI-F058.
* 品単２
     MOVE     CSV-F09           TO        ZAI-F059.
* 品単３
     MOVE     CSV-F10           TO        ZAI-F05A.
* 作業系_出庫棚番
* 作業系_入庫棚番
* 作業系_ストック_（元）
* 作業系_ストック_（先）
* 作業系_数量
* 作業系_備考
* 作業系_未出庫ＦＬＧ
* 入出庫系_出庫棚番
     MOVE     CSV-F11           TO        ZAI-F05I.
* 入出庫系_入庫棚番
     IF       CSV-F02           =        "I3"
              MOVE   CSV-F12    TO        ZAI-F05J
     END-IF.
* 入出庫系_ストック_
     MOVE     CSV-F13           TO        ZAI-F05K.
* 入出庫系_数量
     MOVE     CSV-F14           TO        ZAI-F05L.
* 入出庫系_備考
     MOVE     CSV-F15           TO        ZAI-F05M.
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        ZAI-F060
          MOVE    ERR-KBN(01)   TO        ZAI-F061
          MOVE    ERR-KBN(02)   TO        ZAI-F062
          MOVE    ERR-KBN(03)   TO        ZAI-F063
          MOVE    ERR-KBN(04)   TO        ZAI-F064
          MOVE    ERR-KBN(05)   TO        ZAI-F065
          MOVE    ERR-KBN(06)   TO        ZAI-F066
          MOVE    ERR-KBN(07)   TO        ZAI-F067
          MOVE    ERR-KBN(08)   TO        ZAI-F068
          MOVE    ERR-KBN(09)   TO        ZAI-F069
          MOVE    ERR-KBN(10)   TO        ZAI-F06A
     END-IF.
*
     WRITE    ZAI-REC.
     ADD      1                  TO   ZAI-CNT.
*
 WRITE-ZAI-EXIT.
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
*               倉庫マスタ検索
****************************************************************
 SOKO-READ-SEC               SECTION.
*
     MOVE     "SOKO-READ-SEC"     TO   S-NAME.
*
     READ      ZSOKMS1
       INVALID
               MOVE    "INV"      TO   SOK-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   SOK-INV-FLG
     END-READ.
*
 SOKO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      CNT-55       TO    MSG-OUT02.
     MOVE      CNT-58       TO    MSG-OUT03.
     MOVE      CNT-I3       TO    MSG-OUT04.
     MOVE      CNT-46       TO    MSG-OUT05.
     MOVE      ZAI-CNT      TO    MSG-OUT06.
     MOVE      ERR-CNT      TO    MSG-OUT07.
     MOVE      UNKNOWN      TO    MSG-OUT08.
     MOVE      OK-CNT       TO    MSG-OUT09.
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
     DISPLAY   MSG-OUT8-FIL1 MSG-OUT8-FIL2 MSG-OUT08
               MSG-OUT8-FIL3 MSG-OUT8-FIL4 UPON CONS.

     DISPLAY   MSG-OUT6-FIL1 MSG-OUT6-FIL2 MSG-OUT06
               MSG-OUT6-FIL3 MSG-OUT6-FIL4 UPON CONS.
     DISPLAY   MSG-OUT9-FIL1 MSG-OUT9-FIL2 MSG-OUT09
               MSG-OUT9-FIL3 MSG-OUT9-FIL4 UPON CONS.
     DISPLAY   MSG-OUT7-FIL1 MSG-OUT7-FIL2 MSG-OUT07
               MSG-OUT7-FIL3 MSG-OUT7-FIL4 UPON CONS.
*
     CLOSE     ZAINSKXX
               ZAIWKXX
               JYOKEN1
               ZSOKMS1
               MEIMS1.
*
*ＯＵＴパラメタセット
* 取込件数
     MOVE      CSV-CNT      TO    LINK-OUT-CNT1.
* 棚移動件数(この項目は別ＰＧにてカウントされる)
     MOVE      CNT-55       TO    LINK-OUT-CNT2.
* ストック_ＣＨＧ件数(この項目は別ＰＧにてカウントされる)
     MOVE      CNT-58       TO    LINK-OUT-CNT3.
* 製品在庫移動件数
     MOVE      CNT-I3       TO    LINK-OUT-CNT4.
* 廃棄件数
     MOVE      CNT-46       TO    LINK-OUT-CNT5.
* エラー件数
     MOVE      ERR-CNT      TO    LINK-OUT-CNT6.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
