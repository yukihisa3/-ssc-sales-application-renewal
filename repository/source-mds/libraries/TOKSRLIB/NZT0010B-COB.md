# NZT0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NZT0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　在庫　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　在庫調整データ取込＆更新　　　　　*
*    　　　　　　　　　　　　※Ｄ３６５連携対応　　　　　　　　*
*    作成日／作成者　　　：　2020/05/20 INOUE                  *
*    処理内容　　　　　　：　Ｄ３６５より受け取った在庫調整　　*
*    　　　　　　　　　　　　データについて、整合性チェック　　*
*    　　　　　　　　　　　　・件数カウントを行う。　　　　　　*
*    変更日／作成者　　　：　2021/07/15 TAKAHASHI              *
*    変更内容　　　　　　：  Ｄ３６５連携番号空白対象外　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NZT0010B.
*    　　　　　　　流用元：　TAB0010B.TOKSRLIB
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
*在庫調整データ
     SELECT      RCVZATF     ASSIGN    TO       DA-01-S-RCVZATF
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   RCV-ST.
*在庫調整累積データ　　
     SELECT      ZAITYOL1    ASSIGN    TO       DA-01-VI-ZAITYOL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      RUI-F01  RUI-F02
                                                RUI-F030 RUI-F031
                                                WITH  DUPLICATES
                             FILE      STATUS   RUI-ST.
*売上伝票ファイル
     SELECT      SHTDENLQ    ASSIGN    TO       DA-01-VI-SHTDENLQ
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      DEN-D99  DEN-F03
                             FILE      STATUS   DEN-ST.
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
*在庫調整データ
 FD  RCVZATF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    20        RECORDS.
     COPY        RCVZATF     OF        XFDLIB
     JOINING     RCV         AS        PREFIX.
*在庫調整累積データ
 FD  ZAITYOL1.
     COPY        ZAITYOL1    OF        XFDLIB
     JOINING     RUI         AS        PREFIX.
*売上伝票ファイル
 FD  SHTDENLQ.
     COPY        SHTDENLQ     OF        XFDLIB
     JOINING     DEN          AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL7.
     COPY        SUBMEIL7    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  RCV-ST              PIC  X(02)  VALUE  SPACE.
     03  RUI-ST              PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  RCV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  RCV-CNT             PIC  9(07)  VALUE  ZERO.
     03  RUI-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
     03  CAL-RCV-F11         PIC S9(11)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  10.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-RCV-F03              PIC  X(04)  VALUE SPACE.
 01  WK-RCV-F03-X            REDEFINES   WK-RCV-F03.
     03  WK-RCV-F03R         PIC  9(04).
*
 01  DEN-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
     03  RCV-ERR            PIC  N(10)  VALUE
                   NC"在庫調整データ異常".
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"在庫調整累積Ｆ　異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"売上伝票ファイル異常".
     03  SUB-ERR             PIC  N(10)  VALUE
                   NC"サブ名称マスタ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NZT0010B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NZT0010B".
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
                             NC"累積ファイル　作成　＝".
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
*----<< 在庫更新サブルーチン用 >>--*
 01  ZAIKO-UPDATE-AREA.
     03  ZAIKO-MODE          PIC  X(01)     VALUE  "1".
     03  ZAIKO-TORICD        PIC  9(08).
     03  ZAIKO-AITESHOHINCD  PIC  X(13).
     03  ZAIKO-SOKOCD        PIC  X(02).
     03  ZAIKO-SHOHINCD      PIC  X(08).
     03  ZAIKO-HINTANCD      PIC  X(08).
     03  ZAIKO-NOHINBI       PIC  9(08).
     03  HENKOMAE-SURYO      PIC  9(09).
     03  HENKOGO-SURYO       PIC  9(09)     VALUE  ZERO.
     03  ZAIKO-TANABAN       PIC  X(06).
     03  ZAIKO-HIKIATE-FLG   PIC  X(01).
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
*01  LINK-IN-BUMON               PIC  X(04).
*01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
 01  LINK-OUT-CNT3               PIC  9(07).
 01  LINK-OUT-TRDATE             PIC  9(08).
 01  LINK-OUT-TRTIME             PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-OUT-CNT1
                                             LINK-OUT-CNT2
                                             LINK-OUT-CNT3
                                             LINK-OUT-TRDATE
                                             LINK-OUT-TRTIME.
****************************************************************
 DECLARATIVES.
 RCV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVZATF.
     DISPLAY     RCV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RCV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 RUI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAITYOL1.
     DISPLAY     RUI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RUI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENLQ.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
*ZAI-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
*    DISPLAY     ZAI-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     ZAI-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    STOP        RUN.
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
     OPEN        INPUT       RCVZATF  SUBMEIL7.
     OPEN        I-O         ZAITYOL1 SHTDENLQ.
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
* ＩＮファイル読込
     PERFORM  READ-RCV-SEC.
     IF       RCV-ENDFLG  =   SPACE
              CONTINUE
     ELSE
              DISPLAY NC"在庫調整データ０件です！" UPON CONS
              MOVE     4000      TO       PROGRAM-STATUS
              MOVE    "END"      TO       END-FLG
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-000.
     IF       RCV-ENDFLG  =   SPACE
              PERFORM         HENSYU-RCV-SEC
              GO         TO   MAIN-100
     ELSE
              GO         TO   MAIN-999
     END-IF.
*
 MAIN-100.
* ＩＮファイル読込
     PERFORM  READ-RCV-SEC.
     GO       TO   MAIN-000.
*
 MAIN-999.
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
* ＩＮファイル順読込
****************************************************************
 READ-RCV-SEC                SECTION.
     MOVE     "READ-RCV-SEC"      TO   S-NAME.
*
     READ     RCVZATF   AT   END
              MOVE      "Y"       TO   RCV-ENDFLG
              GO                  TO   READ-RCV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   RCV-CNT.
*
     IF  RCV-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " RCV-CNT  UPON  CONS
     END-IF.
*#2021/07/15 NAV ST Ｄ３６５連携番号が空白の場合は読み飛ばし
     IF  RCV-F01  =  SPACE
         GO                       TO   READ-RCV-SEC
     END-IF.
*#2021/07/15 NAV ED Ｄ３６５連携番号が空白の場合は読み飛ばし
*
 READ-RCV-EXIT.
     EXIT.
***************************************************************
*ＩＮファイルチェック編集
***************************************************************
 HENSYU-RCV-SEC             SECTION.
*
     MOVE    "HENSYU-RCV-SEC"      TO   S-NAME.
*
     MOVE     SPACE                TO   WK-ERR-KBN.
     INITIALIZE                         WK-ERR-TBL.
*
*データ項目チェック
     PERFORM   DATA-CHK-SEC.
*
*売上伝票ファイル更新・（在庫マスタ更新）
     IF        DEN-INV-FLG    =    SPACE
               PERFORM   DEN-UPDT-SEC
     END-IF.
*
*累積ファイル出力
     PERFORM   RUI-WRITE-SEC.
*
 HENSYU-RCV-EXIT.
     EXIT.
***************************************************************
*             データ項目チェック
***************************************************************
 DATA-CHK-SEC     SECTION.
*
     MOVE   "DATA-CHK-SEC"       TO   S-NAME.
*
*------------------------------------------*
*DATA-CHK-00.
*【必須項目チェック】
*　ＥＸＣＥＬでチェックするためノーチェック
*------------------------------------------*
*  登録修正・削除モード：必須
*    IF  RCV-F01  =   "1" OR "2"
*       倉庫コード
*        IF   RCV-F02 =  SPACE
*             MOVE   "Y"          TO        WK-ERR-KBN
*             MOVE   "1"          TO        ERR-KBN(??)
*        END-IF
*       Ｄ３６５ＪＡＮＣＤ
*        IF   RCV-F03 =  SPACE
*             MOVE   "Y"          TO        WK-ERR-KBN
*             MOVE   "1"          TO        ERR-KBN(??)
*        END-IF
*
*------------------------------------------*
*DATA-CHK-01.
*【倉庫商品別_番設定マスタ　存在チェック】
*------------------------------------------*
*    MOVE     RCV-F02        TO   STA-F01.
*    MOVE     RCV-F03        TO   STA-F02.
*    PERFORM  STA-READ-SEC.
*    削除モード時に非存在はエラー
*    IF       RCV-F01         =   "2"
*             IF  STA-INV-FLG =   "INV"
*                 MOVE  "Y"  TO   WK-ERR-KBN
*                 MOVE  "1"  TO   ERR-KBN(01)
*             END-IF
*    END-IF.
*
*------------------------------------------*
 DATA-CHK-01.
*【売上伝票ファイル存在チェック】
*【売上計上済み存在チェック】
*【受注数＜調整数チェック】
*【連携_一致チェック】
*------------------------------------------*
     MOVE     RCV-F01(1:15)  TO   DEN-D99.
     MOVE     RCV-F02        TO   DEN-F03.
     PERFORM  DEN-READ-SEC.
*T
*    DISPLAY "RCV-F01     =" RCV-F01 "F02=" RCV-F02 UPON CONS.
*    DISPLAY " DEN-INV-FLG=" DEN-INV-FLG            UPON CONS.
*T
*
*    非存在はエラー
     IF       DEN-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(01)
*T
*             DISPLAY " WK-ERR-KBN =" WK-ERR-KBN   UPON CONS
*             DISPLAY " ERR-KBN(01)=" ERR-KBN(01)  UPON CONS
*T
              GO             TO   DATA-CHK-05
     END-IF.
*    売上計上済みはエラー
*T
*    DISPLAY " DEN-F277   =" DEN-F277      UPON CONS.
*T
     IF       DEN-F277        =   9
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(03)
*T
*             DISPLAY " WK-ERR-KBN =" WK-ERR-KBN    UPON CONS
*             DISPLAY " ERR-KBN(03)=" ERR-KBN(03)   UPON CONS
*T
     END-IF.
*
*    受注数＜調整数はエラー
*T
*    DISPLAY " RCV-F11    =" RCV-F11                UPON CONS.
*T
     COMPUTE  CAL-RCV-F11     =   RCV-F11   /   100.
     IF       RCV-F10         =  "-"
              COMPUTE  CAL-RCV-F11     =   CAL-RCV-F11   *   -1
     END-IF.
*T
*             DISPLAY " CAL-RCV-F11=" CAL-RCV-F11   UPON CONS
*             DISPLAY " DEN-F50    =" DEN-F50       UPON CONS
*T
     IF       DEN-F50         <   CAL-RCV-F11
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(04)
*T
*             DISPLAY " WK-ERR-KBN =" WK-ERR-KBN    UPON CONS
*             DISPLAY " ERR-KBN(04)=" ERR-KBN(04)   UPON CONS
*T
     END-IF.
*
*    連携_不一致はエラー
*T
*    DISPLAY " RCV-F11    =" RCV-F11                UPON CONS.
*    DISPLAY " DEN-F33    =" DEN-F33                UPON CONS.
*T
     IF       DEN-F33    NOT  =   RCV-F03
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(05)
*T
*             DISPLAY " WK-ERR-KBN =" WK-ERR-KBN    UPON CONS
*             DISPLAY " ERR-KBN(05)=" ERR-KBN(05)   UPON CONS
*T
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【サブ商品名称マスタ存在チェック】
*------------------------------------------*
     MOVE     RCV-F07        TO   SUB-D01.
     PERFORM  SUB-READ-SEC.
*T
*    DISPLAY "RCV-F07     =" RCV-F07                UPON CONS.
*    DISPLAY " SUB-INV-FLG=" SUB-INV-FLG            UPON CONS.
*T
*
*    非存在はエラー
     IF       SUB-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(02)
*T
*             DISPLAY " WK-ERR-KBN =" WK-ERR-KBN    UPON CONS
*             DISPLAY " ERR-KBN(02)=" ERR-KBN(02)   UPON CONS
*T
     END-IF.
*
*------------------------------------------*
*DATA-CHK-04.
*【整合性チェック】
*------------------------------------------*
*    MOVE     RCV-F02        TO   RUI-F112.
*    MOVE     RCV-F03        TO   RUI-F113.
*    MOVE     SPACE          TO   RUI-F20.
*    START    ZAITYOL1   KEY  IS  >=   RUI-F112
*                                      RUI-F113
*                                      RUI-F20
*      INVALID
*        GO        TO    END-SEC-04
*    END-START.
*
*READ-SEC-04.
*    READ     ZAITYOL1   NEXT     AT   END
*             GO         TO    END-SEC-04
*    END-READ.
*
*UPDATA-SEC-04.
*
*    IF    (  RUI-F112    = RCV-F02 ) AND
*          (  RUI-F113    = RCV-F03 )
*             MOVE   "Y"          TO        WK-ERR-KBN
*             MOVE   "1"          TO        ERR-KBN(04)
*             IF         RUI-F20  NOT = "1"
*                        ADD       1     TO        ERR-CNT
*                        SUBTRACT  1     FROM      OK-CNT
*             END-IF
*             MOVE      "1"        TO    RUI-F20  RUI-F204
*             MOVE      SPACE      TO    RUI-F20A
*             REWRITE    RUI-REC
*             READ       ZAITYOL1
*                  NEXT  AT   END
*                        GO        TO    END-SEC-04
*                  NOT   AT   END
*                        GO        TO    READ-SEC-04
*             END-READ
*    END-IF.
*
*END-SEC-04.
*
*    CLOSE         ZAITYOL1.
*    OPEN     I-O  ZAITYOL1.
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
*    売上伝票ファイル検索
****************************************************************
 DEN-READ-SEC               SECTION.
     MOVE    "DEN-READ-SEC" TO   S-NAME.
*
     READ     SHTDENLQ
       INVALID
              MOVE "INV"     TO   DEN-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   DEN-INV-FLG
     END-READ.
 DEN-READ-EXIT.
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
*  累積ファイル出力
****************************************************************
 RUI-WRITE-SEC               SECTION.
*
     MOVE    "RUI-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        RUI-REC.
     INITIALIZE                           RUI-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        RUI-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        RUI-F02.
* D365伝票番号
     MOVE     RCV-F01           TO        RUI-F030.
* 行番号
     MOVE     RCV-F02           TO        RUI-F031.
* 連携NO
     MOVE     RCV-F03           TO        RUI-F032.
* 出荷日付
     MOVE     RCV-F04           TO        RUI-F033.
* 新商品CD
     MOVE     RCV-F05           TO        RUI-F034.
* 旧商品CD
     MOVE     RCV-F06           TO        RUI-F035.
* JANCD
     MOVE     RCV-F07           TO        RUI-F036.
* 倉庫CD
     MOVE     RCV-F08           TO        RUI-F037.
* 場所CD
     MOVE     RCV-F09           TO        RUI-F038.
* 数量符号
     MOVE     RCV-F10           TO        RUI-F039.
* 数量
     MOVE     RCV-F11           TO        RUI-F03A.
*
* エラー区分
*T
*    DISPLAY " RUI-WRITE WK-ERR-KBN =" WK-ERR-KBN   UPON CONS.
*T
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        RUI-F04
     END-IF.
     MOVE     ERR-KBN(01)       TO        RUI-F05(01).
     MOVE     ERR-KBN(02)       TO        RUI-F05(02).
     MOVE     ERR-KBN(03)       TO        RUI-F05(03).
     MOVE     ERR-KBN(04)       TO        RUI-F05(04).
     MOVE     ERR-KBN(05)       TO        RUI-F05(05).
     MOVE     ERR-KBN(06)       TO        RUI-F05(06).
     MOVE     ERR-KBN(07)       TO        RUI-F05(07).
     MOVE     ERR-KBN(08)       TO        RUI-F05(08).
     MOVE     ERR-KBN(09)       TO        RUI-F05(09).
     MOVE     ERR-KBN(10)       TO        RUI-F05(10).
*
* 売上伝票ファイルより
     IF       DEN-INV-FLG    =    SPACE
*             ＮＡＶＳ取引先ＣＤ
                MOVE    DEN-F01    TO       RUI-F06
*             ＮＡＶＳ伝票番号
                MOVE    DEN-F02    TO       RUI-F07
*             ＮＡＶＳ相殺区分
                MOVE    DEN-F04    TO       RUI-F08
*             ＮＡＶＳ伝票区分
                MOVE    DEN-F051   TO       RUI-F09
*             ＮＡＶＳ店舗ＣＤ
                MOVE    DEN-F07    TO       RUI-F10
*             ＮＡＶＳ納品日
                MOVE    DEN-F112   TO       RUI-F11
*             ＮＡＶＳ行番号
                MOVE    DEN-F03    TO       RUI-F12
*             ＮＡＶＳ小売連携NO
                MOVE    DEN-F33    TO       RUI-F13
*             ＮＡＶＳＪＡＮＣＤ
                MOVE    DEN-F25    TO       RUI-F14
*             ＮＡＶＳ出荷場所
                MOVE    DEN-F08    TO       RUI-F15
*             ＮＡＶＳ振分場所
                MOVE    DEN-F48    TO       RUI-F16
*             ＮＡＶＳ商品名１
                MOVE    DEN-F1421  TO       RUI-F17
*             ＮＡＶＳ商品名２
                MOVE    DEN-F1422  TO       RUI-F18
     END-IF.
*
* 売上更新区分・売上更新日
     IF       WK-ERR-KBN        =   SPACE
*             売上更新区分
                MOVE    "1"        TO       RUI-F19
*             売上更新日
                MOVE     SYS-DATE8  TO      RUI-F20
     END-IF.
*
     WRITE    RUI-REC.
     ADD      1                  TO       RUI-CNT.
*
 WRITE-RUI-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル更新・（在庫マスタ更新）
****************************************************************
 DEN-UPDT-SEC                SECTION.
     MOVE    "DEN-UPDT-SEC"  TO   S-NAME.
*
*T
*    DISPLAY " DEN-UPDT  WK-ERR-KBN =" WK-ERR-KBN   UPON CONS.
*T
     IF       WK-ERR-KBN            =     SPACE
*             在庫調整ＦＬＧ
                MOVE    " "         TO    DEN-D93
*             在庫調整日
                MOVE    SYS-DATE8   TO    DEN-D94
*             在庫マスタ更新判定
*T
*               DISPLAY " DEN-UPDT  CAL-RCV-F11="
*                                      CAL-RCV-F11  UPON CONS
*               DISPLAY "           DEN-F15    ="
*                                      DEN-F15      UPON CONS
*T
                IF      CAL-RCV-F11 NOT = DEN-F15
                        PERFORM  900-ZAIKO-SUB
*                       引当済ＦＬＧ（予備３）
                        MOVE     ZAIKO-HIKIATE-FLG  TO   DEN-F27D
                END-IF
     END-IF.
*
     REWRITE  DEN-REC.
*
 DEN-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    在庫更新サブルーチンＣＡＬＬ                 *
*--------------------------------------------------------------*
 900-ZAIKO-SUB          SECTION.
*
     MOVE     "900-ZAIKO-SUB"     TO    S-NAME.
*
*----<< 変数クリア >>--*
     INITIALIZE                   ZAIKO-UPDATE-AREA.
*
*----<< 項目セット >>--*
*****DISPLAY "DEN-F01 = " DEN-F01 " - DEN-F02 = " DEN-F02 UPON
*****         CONS.
     MOVE     "2"                 TO   ZAIKO-MODE.
     MOVE     DEN-F01             TO   ZAIKO-TORICD.
     MOVE     DEN-F25             TO   ZAIKO-AITESHOHINCD.
     MOVE     DEN-F08             TO   ZAIKO-SOKOCD.
     MOVE     DEN-F1411           TO   ZAIKO-SHOHINCD.
     MOVE     DEN-F1412           TO   ZAIKO-HINTANCD.
     MOVE     DEN-F112            TO   ZAIKO-NOHINBI.
     MOVE     DEN-F15             TO   HENKOMAE-SURYO.
     MOVE     CAL-RCV-F11         TO   HENKOGO-SURYO.
     MOVE     DEN-F49             TO   ZAIKO-TANABAN.
     MOVE     DEN-F27D            TO   ZAIKO-HIKIATE-FLG.
*T
*    DISPLAY " ZAIKO-MODE           =" ZAIKO-MODE   UPON CONS.
*    DISPLAY " ZAIKO-TORICD         =" ZAIKO-TORICD UPON CONS.
*    DISPLAY " ZAIKO-AITESHOHINCD=" ZAIKO-AITESHOHINCD UPON CONS.
*    DISPLAY " ZAIKO-SOKOCD      =" ZAIKO-SOKOCD       UPON CONS.
*    DISPLAY " ZAIKO-SHOHINCD    =" ZAIKO-SHOHINCD     UPON CONS.
*    DISPLAY " ZAIKO-HINTANCD    =" ZAIKO-HINTANCD     UPON CONS.
*    DISPLAY " ZAIKO-NOHINBI     =" ZAIKO-NOHINBI      UPON CONS.
*    DISPLAY " HENKOMAE-SURYO    =" HENKOMAE-SURYO     UPON CONS.
*    DISPLAY " HENKOGO-SURYO     =" HENKOGO-SURYO      UPON CONS.
*    DISPLAY " ZAIKO-TANABAN     =" ZAIKO-TANABAN      UPON CONS.
*    DISPLAY " ZAIKO-HIKIATE-FLG =" ZAIKO-HIKIATE-FLG  UPON CONS.
*T
*
*----<< サブルーチンコール >>--*
     CALL    "SJK9010B" USING     ZAIKO-MODE
                                  ZAIKO-TORICD
                                  ZAIKO-AITESHOHINCD
                                  ZAIKO-SOKOCD
                                  ZAIKO-SHOHINCD
                                  ZAIKO-HINTANCD
                                  ZAIKO-NOHINBI
                                  HENKOMAE-SURYO
                                  HENKOGO-SURYO
                                  ZAIKO-TANABAN
                                  ZAIKO-HIKIATE-FLG.
*T
*    DISPLAY " ZAIKO-HIKIATE-FLG =" ZAIKO-HIKIATE-FLG  UPON CONS.
*T
 900-ZAIKO-SUB-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      RCV-CNT      TO    MSG-OUT01.
     MOVE      RUI-CNT      TO    MSG-OUT02.
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
     CLOSE     RCVZATF
               ZAITYOL1
               SHTDENLQ
               SUBMEIL7.
*
*ＯＵＴパラメタセット
* 取込件数
     MOVE      RCV-CNT      TO    LINK-OUT-CNT1.
* ＯＫ件数
     MOVE      OK-CNT       TO    LINK-OUT-CNT2.
* エラー件数
     MOVE      ERR-CNT      TO    LINK-OUT-CNT3.
* 取込日付
     MOVE      SYS-DATE8    TO    LINK-OUT-TRDATE.
* 取込時刻
     MOVE      WK-TIME-HM   TO    LINK-OUT-TRTIME.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
