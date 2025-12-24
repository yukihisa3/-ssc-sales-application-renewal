# TAB0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/TAB0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　倉庫商品別_番設定データ　　　　　*
*    　　　　　　　　　　　　　取込チェック　　　　　　　　　　*
*    　　　　　　　　　　　　※Ｄ３６５連携対応　　　　　　　　*
*    作成日／作成者　　　：　2020/04/22 INOUE                  *
*    処理内容　　　　　　：　倉庫商品別_番設定マスタメンテ　　*
*    　　　　　　　　　　　　データについて、整合性チェック　　*
*    　　　　　　　　　　　　・件数カウントを行う。　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 TAB0010B.
*    　　　　　　　流用元：　NVM0220B
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
*倉庫商品別_番設定マスタ　メンテナンスＣＳＶ
     SELECT      SYOTANCS    ASSIGN    TO       DA-01-S-SYOTANCS
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*倉庫商品別_番設定マスタ　取込ファイル
     SELECT      SYOTANW1    ASSIGN    TO       DA-01-VI-SYOTANW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      WRK-F112 WRK-F113
                                                WRK-F20
                                                WITH  DUPLICATES
                             FILE      STATUS   WRK-ST.
*倉庫マスタ
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
*倉庫商品別_番設定マスタ
     SELECT      SOKTANL1    ASSIGN    TO       DA-01-VI-SOKTANL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      STA-F01
                                                STA-F02
                             FILE      STATUS   STA-ST.
*サブ商品名称マスタ
     SELECT      SUBMEIL7    ASSIGN    TO       DA-01-VI-SUBMEIL7
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SUB-D01
                             FILE      STATUS   SUB-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*倉庫商品別_番設定マスタ　メンテナンスＣＳＶ
 FD  SYOTANCS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    38        RECORDS.
     COPY        SYOTANCS    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*倉庫商品別_番設定マスタ　取込ファイル
 FD  SYOTANW1.
     COPY        SYOTANW1    OF        XFDLIB
     JOINING     WRK         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*倉庫商品別_番設定マスタ
 FD  SOKTANL1.
     COPY        SOKTANL1    OF        XFDLIB
     JOINING     STA         AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL7.
     COPY        SUBMEIL7    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  WRK-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  STA-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  WRK-CNT             PIC  9(07)  VALUE  ZERO.
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
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  STA-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SUB-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
     03  WRK-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
     03  STA-ERR             PIC  N(14)  VALUE
                   NC"倉庫商品別_番設定マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  SUB-ERR             PIC  N(10)  VALUE
                   NC"サブ名称マスタ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "TAB0010B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "TAB0010B".
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
     USE         AFTER       EXCEPTION PROCEDURE SYOTANCS.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 WRK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SYOTANW1.
     DISPLAY     WRK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     WRK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 STA-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SOKTANL1.
     DISPLAY     STA-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     STA-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL7.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SUB-ST      UPON      CONS.
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
     OPEN        INPUT       SYOTANCS ZSOKMS1
                             SOKTANL1 SUBMEIL7.
     OPEN        I-O         SYOTANW1.
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
     DISPLAY "# HIDUKE = " WK-DATE8    UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM  UPON CONS.
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
* メンテナンスＣＳＶ読込
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
* メンテナンスＣＳＶ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     SYOTANCS   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
     IF  CSV-F01  = " "  AND  CSV-F02 = " "
         GO       TO          READ-CSV-SEC
     END-IF.
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
*メンテナンスＣＳＶチェック編集
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
*取込ファイル出力
     PERFORM   WRK-WRITE-SEC.
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
*　ＥＸＣＥＬでチェックするためノーチェック
*------------------------------------------*
*  登録修正・削除モード：必須
*    IF  CSV-F01  =   "1" OR "2"
*       倉庫コード
*        IF   CSV-F02 =  SPACE
*             MOVE   "Y"          TO        WK-ERR-KBN
*             MOVE   "1"          TO        ERR-KBN(??)
*        END-IF
*       Ｄ３６５ＪＡＮＣＤ
*        IF   CSV-F03 =  SPACE
*             MOVE   "Y"          TO        WK-ERR-KBN
*             MOVE   "1"          TO        ERR-KBN(??)
*        END-IF
*
*------------------------------------------*
 DATA-CHK-01.
*【倉庫商品別_番設定マスタ　存在チェック】
*------------------------------------------*
     MOVE     CSV-F02        TO   STA-F01.
     MOVE     CSV-F03        TO   STA-F02.
     PERFORM  STA-READ-SEC.
*    削除モード時に非存在はエラー
     IF       CSV-F01         =   "2"
              IF  STA-INV-FLG =   "INV"
                  MOVE  "Y"  TO   WK-ERR-KBN
                  MOVE  "1"  TO   ERR-KBN(01)
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【サブ商品名称マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   SUB-D01.
     PERFORM  SUB-READ-SEC.
*
*    非存在はエラー
     IF       SUB-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(02)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-03.
*【倉庫マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F02        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
*
*    非存在はエラー
     IF       SOK-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(03)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-04.
*【整合性チェック】
*------------------------------------------*
     MOVE     CSV-F02        TO   WRK-F112.
     MOVE     CSV-F03        TO   WRK-F113.
     MOVE     SPACE          TO   WRK-F20.
     START    SYOTANW1   KEY  IS  >=   WRK-F112
                                       WRK-F113
                                       WRK-F20
       INVALID
         GO        TO    END-SEC-04
     END-START.
*
 READ-SEC-04.
     READ     SYOTANW1   NEXT     AT   END
              GO         TO    END-SEC-04
     END-READ.
*
 UPDATA-SEC-04.
*
     IF    (  WRK-F112    = CSV-F02 ) AND
           (  WRK-F113    = CSV-F03 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
              IF         WRK-F20  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    WRK-F20  WRK-F204
              MOVE      SPACE      TO    WRK-F20A
              REWRITE    WRK-REC
              READ       SYOTANW1
                   NEXT  AT   END
                         GO        TO    END-SEC-04
                   NOT   AT   END
                         GO        TO    READ-SEC-04
              END-READ
     END-IF.
*
 END-SEC-04.
*
     CLOSE         SYOTANW1.
     OPEN     I-O  SYOTANW1.
*
*------------------------------------------*
 DATA-CHK-05.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD     1        TO     ERR-CNT
     ELSE
              ADD     1        TO     OK-CNT
              MOVE   "1"       TO     ERR-KBN(10)
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*    倉庫商品別_番設定マスタ検索
****************************************************************
 STA-READ-SEC               SECTION.
     MOVE    "STA-READ-SEC" TO   S-NAME.
*
     READ     SOKTANL1
       INVALID
              MOVE "INV"     TO   STA-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   STA-INV-FLG
     END-READ.
 STA-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ検索
****************************************************************
 SUB-READ-SEC               SECTION.
     MOVE    "SUB-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL7
       INVALID
              MOVE "INV"     TO   SUB-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SUB-INV-FLG
     END-READ.
 SUB-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
     MOVE    "SOK-READ-SEC" TO   S-NAME.
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
*  取込ファイル出力
****************************************************************
 WRK-WRITE-SEC               SECTION.
*
     MOVE    "WRK-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        WRK-REC.
     INITIALIZE                           WRK-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        WRK-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        WRK-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        WRK-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        WRK-F04.
* 登録・更新・削除区分
     MOVE     CSV-F01           TO        WRK-F10.
* 削除区分
     MOVE     CSV-F01           TO        WRK-F111.
* 倉庫コード
     MOVE     CSV-F02           TO        WRK-F112.
* Ｄ３６５ＪＡＮＣＤ
     MOVE     CSV-F03           TO        WRK-F113.
* 商品名１
     MOVE     CSV-F04           TO        WRK-F114.
* 商品名２
     MOVE     CSV-F05           TO        WRK-F115.
* _番
     MOVE     CSV-F06           TO        WRK-F116.
* _番改定日
     MOVE     CSV-F07           TO        WRK-F117.
* 改定_番
     MOVE     CSV-F08           TO        WRK-F118.
*
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        WRK-F20
     END-IF.
     MOVE     ERR-KBN(01)       TO        WRK-F201.
     MOVE     ERR-KBN(02)       TO        WRK-F202.
     MOVE     ERR-KBN(03)       TO        WRK-F203.
     MOVE     ERR-KBN(04)       TO        WRK-F204.
     MOVE     ERR-KBN(05)       TO        WRK-F205.
     MOVE     ERR-KBN(06)       TO        WRK-F206.
     MOVE     ERR-KBN(07)       TO        WRK-F207.
     MOVE     ERR-KBN(08)       TO        WRK-F208.
     MOVE     ERR-KBN(09)       TO        WRK-F209.
     MOVE     ERR-KBN(10)       TO        WRK-F20A.
*
     WRITE    WRK-REC.
     ADD      1                  TO       WRK-CNT.
*
 WRITE-WRK-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      WRK-CNT      TO    MSG-OUT02.
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
     CLOSE     SYOTANCS
               SYOTANW1
               ZSOKMS1
               SOKTANL1
               SUBMEIL7.
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

```
