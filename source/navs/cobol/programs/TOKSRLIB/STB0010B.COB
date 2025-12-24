****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：　商品変換テーブル取込チェック　*
*    作成日／作成者　　　：　2017/12/04 INOUE                  *
*    処理内容　　　　　　：　商品変換テーブルメンテデータ　*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    流用元　　　　　　　：　SSY3930B                          *
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 STB0010B.
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
*商品変換テーブルメンテナンスＣＳＶ
     SELECT      STBXXXCS    ASSIGN    TO       DA-01-S-STBXXXCS
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*商品変換テーブル取込ファイル
     SELECT      STBXXXW1    ASSIGN    TO       DA-01-VI-STBXXXW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      STB-F13  STB-F14
                                                STB-F060
                                                WITH  DUPLICATES
                             FILE      STATUS   STB-ST.
*商品変換テーブル
     SELECT      SHOTBL1     ASSIGN    TO       DA-01-VI-SHOTBL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SHO-F01 SHO-F02
                             FILE      STATUS   SHO-ST.
*取引先マスタ
     SELECT      TOKMS2      ASSIGN    TO       DA-01-VI-TOKMS2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TOK-F01
                             FILE      STATUS   TOK-ST.
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
*商品変換テーブルメンテナンスＣＳＶ
 FD  STBXXXCS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    35        RECORDS.
     COPY        STBXXXCS    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*商品変換テーブル取込ファイル
 FD  STBXXXW1.
     COPY        STBXXXW1    OF        XFDLIB
     JOINING     STB         AS        PREFIX.
*商品変換テーブル
 FD  SHOTBL1.
     COPY        SHOTBL1     OF        XFDLIB
     JOINING     SHO         AS        PREFIX.
*取引先マスタ
 FD  TOKMS2.
     COPY        TOKMS2      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
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
     03  STB-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  STB-CNT             PIC  9(07)  VALUE  ZERO.
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
 01  SHO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  TOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
 01  FILE-ERR.
     03  CSV-ERR            PIC  N(10)  VALUE
                   NC"メンテＣＳＶ異常".
     03  STB-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "STB0010B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "STB0010B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"メンテデータ　読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル　作成　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
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
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-OUT-CNT1
                                             LINK-OUT-CNT2.
****************************************************************
 DECLARATIVES.
 CSV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE STBXXXCS.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 STB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE STBXXXW1.
     DISPLAY     STB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     STB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TOKMS2.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
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
     OPEN        INPUT       STBXXXCS SHOTBL1 TOKMS2
                             MEIMS1   ZSOKMS1.
     OPEN        I-O         STBXXXW1.
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
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 商品変換テーブルメンテナンスＣＳＶ読込
* 終了するまで繰りかえす
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
* 商品変換テーブルメンテナンスＣＳＶ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     STBXXXCS   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   CSV-CNT.
*
     IF  CSV-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " CSV-CNT  UPON  CONS
     END-IF.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*商品変換テーブルメンテナンスＣＳＶチェック編集
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
*商品変換テーブル取込ファイル出力
     PERFORM   STB-WRITE-SEC.
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
*------------------------------------------*
 DATA-CHK-00.
*【必須項目チェック】
*------------------------------------------*
*  登録モード時は必須
     IF  CSV-F01  =   "Y"
*       自社商品コード
         IF   CSV-F06 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(06)
         END-IF
*       品単１・２・３
         IF ( CSV-F08 =  SPACE ) AND
            ( CSV-F10 =  SPACE ) AND
            ( CSV-F12 =  SPACE )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(06)
         END-IF
*       出荷場所
         IF   CSV-F14 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(06)
         END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-01.
*【取引先ＣＤ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   TOK-F01.
     PERFORM  TOK-READ-SEC.
     IF       TOK-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(01)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【商品変換ＴＢＬ存在チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   SHO-F01.
     MOVE     CSV-F04        TO   SHO-F02.
     PERFORM  SHO-READ-SEC.
*             登録モード時に存在はエラー
     IF       CSV-F01         =   "Y"
              IF  SHO-INV-FLG =   "   "
                  MOVE  "Y"  TO   WK-ERR-KBN
                  MOVE  "1"  TO   ERR-KBN(02)
              END-IF
     END-IF.
*             更新モード時に非存在はエラー
     IF       CSV-F02         =   "Y"
              IF  SHO-INV-FLG =   "INV"
                  MOVE  "Y"  TO   WK-ERR-KBN
                  MOVE  "1"  TO   ERR-KBN(02)
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-03.
*【商品名称マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F06        TO   MEI-F011.
     MOVE     CSV-F08        TO   MEI-F0121.
     MOVE     CSV-F10        TO   MEI-F0122.
     MOVE     CSV-F12        TO   MEI-F0123.
     PERFORM  MEI-READ-SEC.
*   以下の場合のみチェック
*   (登録モード)OR
*   (更新モード AND「自社商品ＣＤ」～「品単３」いずれか変更)
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F05         =   " " ) AND
            ( CSV-F07         =   " " ) AND
            ( CSV-F09         =   " " ) AND
            ( CSV-F11         =   " " )
              GO             TO   DATA-CHK-04
     END-IF.
*
*    非存在はエラー
     IF       MEI-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(03)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-04.
*【出荷場所未登録チェック】
*------------------------------------------*
     MOVE     CSV-F14        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
*   以下の場合のみチェック
*   (登録モード)OR
*   (更新モード AND「出荷場所変更)
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F14         =   " " )
              GO             TO   DATA-CHK-05
     END-IF.
*
*    非存在はエラー
     IF       SOK-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-05.
*【整合性チェック】
*------------------------------------------*
     MOVE     SPACE          TO   STB-F060.
     MOVE     CSV-F03        TO   STB-F13.
     MOVE     CSV-F04        TO   STB-F14.
     START    STBXXXW1   KEY  IS  >=   STB-F13
                                       STB-F14
                                       STB-F060
       INVALID
         GO        TO    END-SEC-05
     END-START.
*
 READ-SEC-05.
     READ     STBXXXW1   NEXT     AT   END
              GO         TO    END-SEC-05
     END-READ.
*
 UPDATA-SEC-05.
*
     IF    (  STB-F13    = CSV-F03 ) AND
           (  STB-F14    = CSV-F04 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(05)
              IF         STB-F060  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    STB-F060  STB-F065
              MOVE      SPACE      TO    STB-F06A
              REWRITE    STB-REC
              READ       STBXXXW1
                   NEXT  AT   END
                         GO        TO    END-SEC-05
                   NOT   AT   END
                         GO        TO    READ-SEC-05
              END-READ
     END-IF.
*
 END-SEC-05.
*
     CLOSE         STBXXXW1.
     OPEN     I-O  STBXXXW1.
*
*------------------------------------------*
 DATA-CHK-06.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     ERR-CNT
     ELSE
              ADD    1        TO     OK-CNT
              MOVE   "1"      TO     ERR-KBN(10)
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*    商品変換ＴＢＬ検索
****************************************************************
 SHO-READ-SEC               SECTION.
     MOVE     "SHO-READ-SEC" TO   S-NAME.
*
     READ     SHOTBL1
       INVALID
              MOVE "INV"     TO   SHO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHO-INV-FLG
     END-READ.
 SHO-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索
****************************************************************
 TOK-READ-SEC               SECTION.
     MOVE     "TOK-READ-SEC" TO   S-NAME.
*
     READ     TOKMS2
       INVALID
              MOVE "INV"     TO   TOK-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   TOK-INV-FLG
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索
****************************************************************
 MEI-READ-SEC               SECTION.
     MOVE     "MEI-READ-SEC" TO   S-NAME.
*
     READ     MEIMS1
       INVALID
              MOVE "INV"     TO   MEI-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MEI-INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
     MOVE     "SOK-READ-SEC" TO   S-NAME.
*
     READ     ZSOKMS1
       INVALID
              MOVE "INV"     TO   SOK-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SOK-INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
****************************************************************
*  商品変換テーブル取込ファイル出力
****************************************************************
 STB-WRITE-SEC               SECTION.
*
     MOVE    "STB-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        STB-REC.
     INITIALIZE                           STB-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        STB-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        STB-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        STB-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        STB-F04.
* 登録・更新区分
     IF       CSV-F01    =      "Y"
              MOVE      "1"     TO        STB-F11
     END-IF.
     IF       CSV-F02    =      "Y"
              MOVE      "2"     TO        STB-F11
     END-IF.
* 登録ＣＨＫ　
     MOVE     CSV-F01           TO        STB-F121.
* 更新ＣＨＫ
     MOVE     CSV-F02           TO        STB-F122.
* 取引先コード
     MOVE     CSV-F03           TO        STB-F13.
* 量販店商品コード
     MOVE     CSV-F04           TO        STB-F14.
* 【登録モード】
     IF   CSV-F01       =   "Y"
*        自社商品コード
          MOVE    CSV-F06       TO        STB-F152
*        品単１
          MOVE    CSV-F08       TO        STB-F162
*        品単２
          MOVE    CSV-F10       TO        STB-F172
*        品単３
          MOVE    CSV-F12       TO        STB-F182
*        出荷場所
          MOVE    CSV-F14       TO        STB-F192
*        原価単価
          MOVE    CSV-F16       TO        STB-F202
*        売価単価
          MOVE    CSV-F18       TO        STB-F212
*        分類コード
          MOVE    CSV-F20       TO        STB-F222
*        棚番
          MOVE    CSV-F22       TO        STB-F232
*        仕入単価
          MOVE    CSV-F24       TO        STB-F242
*        検品Ｇ
          MOVE    CSV-F26       TO        STB-F252
     END-IF.
*
* 【更新モード】
     IF   CSV-F02       =   "Y"
*        自社商品コード
          MOVE    CSV-F05        TO       STB-F151
          MOVE    CSV-F06        TO       STB-F152
*        品単１
          MOVE    CSV-F07        TO       STB-F161
          MOVE    CSV-F08        TO       STB-F162
*        品単２
          MOVE    CSV-F09        TO       STB-F171
          MOVE    CSV-F10        TO       STB-F172
*        品単３
          MOVE    CSV-F11        TO       STB-F181
          MOVE    CSV-F12        TO       STB-F182
*        出荷場所
          MOVE    CSV-F13        TO       STB-F191
          MOVE    CSV-F14        TO       STB-F192
*        原価単価
          MOVE    CSV-F15        TO       STB-F201
          MOVE    CSV-F16        TO       STB-F202
*        売価単価
          MOVE    CSV-F17        TO       STB-F211
          MOVE    CSV-F18        TO       STB-F212
*        分類コード
          MOVE    CSV-F19        TO       STB-F221
          MOVE    CSV-F20        TO       STB-F222
*        棚番
          MOVE    CSV-F21        TO       STB-F231
          MOVE    CSV-F22        TO       STB-F232
*        仕入単価
          MOVE    CSV-F23        TO       STB-F241
          MOVE    CSV-F24        TO       STB-F242
*        検品Ｇ
          MOVE    CSV-F25        TO       STB-F251
          MOVE    CSV-F26        TO       STB-F252
     END-IF.
*
* 取引先名
     IF   TOK-INV-FLG       =   "   "
          MOVE    TOK-F02       TO        STB-F51
     ELSE
          MOVE    ALL NC"？"    TO        STB-F51
     END-IF.
* 商品名１・２
     IF   MEI-INV-FLG       =   "   "
          MOVE    MEI-F021      TO        STB-F52
          MOVE    MEI-F022      TO        STB-F53
     ELSE
          MOVE    ALL NC"？"    TO        STB-F52
                                          STB-F53
     END-IF.
* 倉庫名
     IF   SOK-INV-FLG       =   "   "
          MOVE    SOK-F02       TO        STB-F54
     ELSE
          MOVE    ALL NC"？"    TO        STB-F54
     END-IF.
*
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        STB-F060
     END-IF.
          MOVE    ERR-KBN(01)   TO        STB-F061.
          MOVE    ERR-KBN(02)   TO        STB-F062.
          MOVE    ERR-KBN(03)   TO        STB-F063.
          MOVE    ERR-KBN(04)   TO        STB-F064.
          MOVE    ERR-KBN(05)   TO        STB-F065.
          MOVE    ERR-KBN(06)   TO        STB-F066.
          MOVE    ERR-KBN(07)   TO        STB-F067.
          MOVE    ERR-KBN(08)   TO        STB-F068.
          MOVE    ERR-KBN(09)   TO        STB-F069.
          MOVE    ERR-KBN(10)   TO        STB-F06A.
*
     WRITE    STB-REC.
     ADD      1                  TO   STB-CNT.
*
 WRITE-STB-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      STB-CNT      TO    MSG-OUT02.
     MOVE      ERR-CNT      TO    MSG-OUT03.
     MOVE      OK-CNT       TO    MSG-OUT04.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
*
     CLOSE     STBXXXCS
               STBXXXW1
               SHOTBL1
               TOKMS2
               MEIMS1
               ZSOKMS1.
*
*ＯＵＴパラメタセット
* 取込件数
     MOVE      CSV-CNT      TO    LINK-OUT-CNT1.
* エラー件数
     MOVE      ERR-CNT      TO    LINK-OUT-CNT2.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
