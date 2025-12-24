# NVM0220B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVM0220B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　商品名称マスタ　取込チェック　　　*
*    　　　　　　　　　　　　※Ｄ３６５連携対応　　　　　　　　*
*    作成日／作成者　　　：　2020/04/10 INOUE                  *
*    処理内容　　　　　　：　商品名称マスタ　メンテデータ　　　*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    変更日／作成者　　　：　2020/05/28 INOUE                  *
*    変更内容　　　　　　：　オーダー区分追加　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVM0220B.
*    　　　　　　　流用元：　NVM0120B
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
*商品名称マスタ　メンテナンスＣＳＶ
     SELECT      SMSMNTCS    ASSIGN    TO       DA-01-S-SMSMNTCS
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*商品名称マスタ　取込ファイル
     SELECT      SMSMNTW1    ASSIGN    TO       DA-01-VI-SMSMNTW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      SMS-F13  SMS-F14
                                                SMS-F060
                                                WITH  DUPLICATES
                             FILE      STATUS   SMS-ST.
*仕入先マスタ
     SELECT      ZSHIMS1     ASSIGN    TO       DA-01-VI-ZSHIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SHI-F01
                             FILE      STATUS   SHI-ST.
*商品名称マスタ
     SELECT      MEIMS1      ASSIGN    TO       DA-01-VI-MEIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F011
                                                MEI-F012
                             FILE      STATUS   MEI-ST.
*サブ商品名称マスタ
     SELECT      SUBMEIL1    ASSIGN    TO       DA-01-VI-SUBMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SUB-F011
                                                SUB-F012
                             FILE      STATUS   SUB-ST.
*サブ商品名称マスタＬ７（同一D365JANCD存在チェック用）
     SELECT      SUBMEIL7  ASSIGN    TO        DA-01-VI-SUBMEIL7
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   SB7-D01
                           FILE      STATUS    IS   SB7-ST.
*サブ商品名称マスタＬ８（同一D365商品CD存在チェック用）
     SELECT      SUBMEIL8  ASSIGN    TO        DA-01-VI-SUBMEIL8
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   SB8-D02
                           FILE      STATUS    IS   SB8-ST.
*条件ファイル
     SELECT      JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   JYO-F01
                                                    JYO-F02
                           FILE      STATUS    IS   JYO-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品名称マスタ　メンテナンスＣＳＶ
 FD  SMSMNTCS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    14        RECORDS.
     COPY        SMSMNTCS    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*商品名称マスタ　取込ファイル
 FD  SMSMNTW1.
     COPY        SMSMNTW1    OF        XFDLIB
     JOINING     SMS         AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS1.
     COPY        ZSHIMS1      OF        XFDLIB
     JOINING     SHI         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL1.
     COPY        SUBMEIL1    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
*サブ商品名称マスタL7
 FD  SUBMEIL7.
     COPY        SUBMEIF     OF        XFDLIB
                 JOINING     SB7       PREFIX.
*サブ商品名称マスタL8
 FD  SUBMEIL8.
     COPY        SUBMEIF     OF        XFDLIB
                 JOINING     SB8       PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  SMS-ST              PIC  X(02)  VALUE  SPACE.
     03  SHI-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
     03  SB7-ST              PIC  X(02)  VALUE  SPACE.
     03  SB8-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  SMS-CNT             PIC  9(07)  VALUE  ZERO.
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
 01  SHI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SUB-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SB7-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SB8-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
     03  SMS-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SHI-ERR             PIC  N(10)  VALUE
                   NC"仕入先マスタ異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  SUB-ERR             PIC  N(10)  VALUE
                   NC"サブ名称マスタ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVM0220B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVM0220B".
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
     USE         AFTER       EXCEPTION PROCEDURE SMSMNTCS.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SMS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SMSMNTW1.
     DISPLAY     SMS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SMS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL1.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SUB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SB7-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL7.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SB7-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SB8-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL8.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SB8-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSHIMS1.
     DISPLAY     SHI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
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
     OPEN        INPUT       SMSMNTCS ZSHIMS1  JYOKEN1
                             MEIMS1   SUBMEIL1 SUBMEIL7 SUBMEIL8.
     OPEN        I-O         SMSMNTW1.
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
* 商品名称マスタ　メンテナンスＣＳＶ読込
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
* 商品名称マスタ　メンテナンスＣＳＶ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     SMSMNTCS   AT   END
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
*商品名称マスタ　メンテナンスＣＳＶチェック編集
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
*商品名称マスタ　取込ファイル出力
     PERFORM   SMS-WRITE-SEC.
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
*       商品コード
         IF   CSV-F03 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*       商品名１
         IF   CSV-F06 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*       主仕入先ＣＤ
         IF   CSV-F20 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*       定番区分
         IF   CSV-F40 NOT NUMERIC
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*       Ｄ３６５商品ＣＤ
         IF   CSV-F42 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*       Ｄ３６５ＪＡＮＣＤ
         IF   CSV-F44 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*2020.05.28↓
*       オーダー区分
         IF   CSV-F46 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*2020.05.28↑
     END-IF.
*
*  更新モード時
     IF  CSV-F02  =   "Y"
*     更新モード時は必須
*       商品コード
         IF   CSV-F03 =  SPACE
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(04)
         END-IF
*     更新モード＆更新ＣＨＫ="Y"時は必須
*       商品名１
         IF   CSV-F05 =  "Y"
              IF  CSV-F06 =  SPACE
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*       主仕入先ＣＤ
         IF   CSV-F19 =  "Y"
              IF  CSV-F20 =  SPACE
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*       定番区分
         IF   CSV-F39 =  "Y"
              IF  CSV-F40 NOT NUMERIC
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*       Ｄ３６５商品ＣＤ
         IF   CSV-F41 =  "Y"
              IF  CSV-F42 =  SPACE
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*       Ｄ３６５ＪＡＮＣＤ
         IF   CSV-F43 =  "Y"
              IF  CSV-F44 =  SPACE
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*2020.05.28↓
*       オーダー区分
         IF   CSV-F45 =  "Y"
              IF  CSV-F46 =  SPACE
                  MOVE   "Y"          TO        WK-ERR-KBN
                  MOVE   "1"          TO        ERR-KBN(04)
              END-IF
         END-IF
*2020.05.28↑
     END-IF.
*
*------------------------------------------*
 DATA-CHK-01.
*【商品名称マスタ　　存在チェック】
*【サブ商品名称マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   MEI-F011.
     MOVE     CSV-F04        TO   MEI-F012.
     PERFORM  MEI-READ-SEC.
     MOVE     CSV-F03        TO   SUB-F011.
     MOVE     CSV-F04        TO   SUB-F012.
     PERFORM  SUB-READ-SEC.
*    登録モード時に両方存在はエラー
     IF       CSV-F01         =   "Y"
              IF  ( MEI-INV-FLG =   "   " ) AND
                  ( SUB-INV-FLG =   "   " )
                  MOVE  "Y"  TO   WK-ERR-KBN
                  MOVE  "1"  TO   ERR-KBN(01)
              END-IF
     END-IF.
*    更新モード時に両方非存在はエラー
     IF       CSV-F02         =   "Y"
              IF  ( MEI-INV-FLG =   "INV" ) AND
                  ( SUB-INV-FLG =   "INV" )
                  MOVE  "Y"  TO   WK-ERR-KBN
                  MOVE  "1"  TO   ERR-KBN(01)
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【仕入先マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F20        TO   SHI-F01.
     PERFORM  SHI-READ-SEC.
*   以下の場合のみチェック
*   (登録モード)OR
*   (更新モード AND 変更あり)
     IF     ( CSV-F02        =   "Y" ) AND
            ( CSV-F19        =   " " )
              GO             TO   DATA-CHK-03
     END-IF.
*
*    非存在はエラー
     IF       SHI-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(02)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-03.
*【区分エラーチェック】
*------------------------------------------*
*  廃盤区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F25         =   " " )
              CONTINUE
     ELSE
*             " ","1","2"以外はエラー
              IF     ( CSV-F26 NOT = " " ) AND
                     ( CSV-F26 NOT = "0" ) AND
                     ( CSV-F26 NOT = "1" ) AND
                     ( CSV-F26 NOT = "2" )
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              END-IF
     END-IF.
*
*  小売区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F29         =   " " )
              CONTINUE
     ELSE
*             " ","1"以外はエラー
              IF     ( CSV-F30 NOT = " " ) AND
                     ( CSV-F30 NOT = "1" )
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              END-IF
     END-IF.
*
*  振替区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F31         =   " " )
              CONTINUE
     ELSE
*             " ","1"以外はエラー
              IF     ( CSV-F32 NOT = " " ) AND
                     ( CSV-F32 NOT = "1" )
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              END-IF
     END-IF.
*
*  物流束区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F33         =   " " )
              CONTINUE
     ELSE
*             " ","1"以外はエラー
              IF     ( CSV-F34 NOT = " " ) AND
                     ( CSV-F34 NOT = "1" )
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              END-IF
     END-IF.
*
*  管理区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F37         =   " " )
              CONTINUE
     ELSE
*             " ","1"以外はエラー
              IF     ( CSV-F38 NOT = " " ) AND
                     ( CSV-F38 NOT = "1" )
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              END-IF
     END-IF.
*
*  定番区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F39         =   " " )
              CONTINUE
     ELSE
*             数値以外はエラー
              IF       CSV-F40 NOT NUMERIC
                       MOVE   "Y"     TO   WK-ERR-KBN
                       MOVE   "1"     TO   ERR-KBN(03)
              ELSE
*                      0,1以外はエラー
                       IF     ( CSV-F40 NOT =  0  ) AND
                              ( CSV-F40 NOT =  1  )
                                MOVE   "Y"     TO   WK-ERR-KBN
                                MOVE   "1"     TO   ERR-KBN(03)
                       END-IF
              END-IF
     END-IF.
*
*  ２０分類区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F27         =   " " )
              CONTINUE
     ELSE
*             空白以外で条件ファイルなしはエラー
              IF       CSV-F28  NOT = "  "
                       MOVE     10        TO   JYO-F01
                       MOVE     CSV-F28   TO   JYO-F02
                       PERFORM  JYO-READ-SEC
                       IF       JYO-INV-FLG  =  "INV"
                                MOVE   "Y"     TO   WK-ERR-KBN
                                MOVE   "1"     TO   ERR-KBN(03)
                       END-IF
              END-IF
     END-IF.
*
*  商品分類区分
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F35         =   " " )
              CONTINUE
     ELSE
*             空白以外で条件ファイルなしはエラー
              IF       CSV-F36  NOT = " "
                       MOVE     91        TO   JYO-F01
                       MOVE     CSV-F36   TO   JYO-F02
                       PERFORM  JYO-READ-SEC
                       IF       JYO-INV-FLG  =  "INV"
                                MOVE   "Y"     TO   WK-ERR-KBN
                                MOVE   "1"     TO   ERR-KBN(03)
                       END-IF
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-05.
*【同一Ｄ３６５商品ＣＤチェック】
*------------------------------------------*
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F41         =   " " )
              CONTINUE
     ELSE
*             同一ＣＤありはエラー
              MOVE     CSV-F42    TO   SB8-D02
              PERFORM  SB8-READ-SEC
              IF       SB8-INV-FLG  =  "   "
*T
*                      DISPLAY "CSV-F42="  CSV-F42    UPON STA
*                      DISPLAY "SUB    ="  SB8-D02    UPON STA
*                      DISPLAY "SHOCD  ="  CSV-F03    UPON STA
*                      DISPLAY "SUB    ="  SB8-F011   UPON STA
*                      DISPLAY "TANCD  ="  CSV-F04    UPON STA
*                      DISPLAY "SUB    ="  SB8-F012   UPON STA
*T
                       IF  ( CSV-F03 = SB8-F011 ) AND
                           ( CSV-F04 = SB8-F012 )
                             CONTINUE
                       ELSE
                             MOVE      "Y"  TO   WK-ERR-KBN
                             MOVE      "1"  TO   ERR-KBN(05)
                       END-IF
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-06.
*【同一Ｄ３６５ＪＡＮＣＤチェック】
*------------------------------------------*
*   (登録モード)OR
*   (更新モード AND 変更あり)のみチェック
     IF     ( CSV-F02         =   "Y" ) AND
            ( CSV-F43         =   " " )
              CONTINUE
     ELSE
*             同一ＣＤありはエラー
              MOVE     CSV-F44    TO   SB7-D01
              PERFORM  SB7-READ-SEC
              IF       SB7-INV-FLG  =  "   "
*T
*                      DISPLAY "CSV-F44="  CSV-F44    UPON STA
*                      DISPLAY "SUB    ="  SB7-D01    UPON STA
*                      DISPLAY "SHOCD  ="  CSV-F03    UPON STA
*                      DISPLAY "SUB    ="  SB7-F011   UPON STA
*                      DISPLAY "TANCD  ="  CSV-F04    UPON STA
*                      DISPLAY "SUB    ="  SB7-F012   UPON STA
*T
                       IF  ( CSV-F03 = SB7-F011 ) AND
                           ( CSV-F04 = SB7-F012 )
                             CONTINUE
                       ELSE
                             MOVE      "Y"  TO   WK-ERR-KBN
                             MOVE      "1"  TO   ERR-KBN(06)
                       END-IF
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-07.
*【整合性チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   SMS-F13.
     MOVE     CSV-F04        TO   SMS-F14.
     MOVE     SPACE          TO   SMS-F060.
     START    SMSMNTW1   KEY  IS  >=   SMS-F13
                                       SMS-F14
                                       SMS-F060
       INVALID
         GO        TO    END-SEC-05
     END-START.
*
 READ-SEC-05.
     READ     SMSMNTW1   NEXT     AT   END
              GO         TO    END-SEC-05
     END-READ.
*
 UPDATA-SEC-05.
*
     IF    (  SMS-F13    = CSV-F03 ) AND
           (  SMS-F14    = CSV-F04 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(07)
              IF         SMS-F060  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    SMS-F060  SMS-F067
              MOVE      SPACE      TO    SMS-F06A
              REWRITE    SMS-REC
              READ       SMSMNTW1
                   NEXT  AT   END
                         GO        TO    END-SEC-05
                   NOT   AT   END
                         GO        TO    READ-SEC-05
              END-READ
     END-IF.
*
 END-SEC-05.
*
     CLOSE         SMSMNTW1.
     OPEN     I-O  SMSMNTW1.
*
*------------------------------------------*
 DATA-CHK-08.
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
*    商品名称マスタ検索
****************************************************************
 MEI-READ-SEC               SECTION.
     MOVE    "MEI-READ-SEC" TO   S-NAME.
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
*    サブ商品名称マスタ検索
****************************************************************
 SUB-READ-SEC               SECTION.
     MOVE    "SUB-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL1
       INVALID
              MOVE "INV"     TO   SUB-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SUB-INV-FLG
     END-READ.
 SUB-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ検索L7 (KEY=D365JANCD)
****************************************************************
 SB7-READ-SEC               SECTION.
     MOVE    "SB7-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL7
       INVALID
              MOVE "INV"     TO   SB7-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SB7-INV-FLG
     END-READ.
 SB7-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ検索L8 (KEY=D365商品CD)
****************************************************************
 SB8-READ-SEC               SECTION.
     MOVE    "SB8-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL8
       INVALID
              MOVE "INV"     TO   SB8-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SB8-INV-FLG
     END-READ.
 SB8-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ検索
****************************************************************
 SHI-READ-SEC               SECTION.
     MOVE    "SHI-READ-SEC" TO   S-NAME.
*
     READ     ZSHIMS1
       INVALID
              MOVE "INV"     TO   SHI-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHI-INV-FLG
     END-READ.
 SHI-READ-EXIT.
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
*  商品名称マスタ　取込ファイル出力
****************************************************************
 SMS-WRITE-SEC               SECTION.
*
     MOVE    "SMS-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        SMS-REC.
     INITIALIZE                           SMS-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        SMS-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        SMS-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        SMS-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        SMS-F04.
* 登録・更新区分
     IF       CSV-F01    =      "Y"
              MOVE      "1"     TO        SMS-F11
     END-IF.
     IF       CSV-F02    =      "Y"
              MOVE      "2"     TO        SMS-F11
     END-IF.
* 登録ＣＨＫ　
     MOVE     CSV-F01           TO        SMS-F121.
* 更新ＣＨＫ
     MOVE     CSV-F02           TO        SMS-F122.
* 商品コード
     MOVE     CSV-F03           TO        SMS-F13.
* 品単コード
     MOVE     CSV-F04           TO        SMS-F14.
*
* 【登録モード】
     IF   CSV-F01       =   "Y"
*        商品漢字名１
          MOVE    CSV-F06       TO        SMS-F152
*        商品漢字名２
          MOVE    CSV-F08       TO        SMS-F162
*        商品カナ名１
          MOVE    CSV-F10       TO        SMS-F172
*        商品カナ名２
          MOVE    CSV-F12       TO        SMS-F182
*        仕入単価
*         MOVE    CSV-F14       TO        SMS-F192
          COMPUTE SMS-F192  =   CSV-F14 / 100
*        基準原価
*         MOVE    CSV-F16       TO        SMS-F202
          COMPUTE SMS-F202  =   CSV-F16 / 100
*        基準売価
*         MOVE    CSV-F18       TO        SMS-F212
          COMPUTE SMS-F212  =   CSV-F18 / 100
*        主仕入先コード
          MOVE    CSV-F20       TO        SMS-F222
*        ＪＡＮコード
          MOVE    CSV-F22       TO        SMS-F232
*        入数
          MOVE    CSV-F24       TO        SMS-F242
*        廃盤区分
          MOVE    CSV-F26       TO        SMS-F252
*        サカタ２０分類
          MOVE    CSV-F28       TO        SMS-F262
*        小売区分
          MOVE    CSV-F30       TO        SMS-F272
*        振替区分
          MOVE    CSV-F32       TO        SMS-F282
*        物流_区分
          MOVE    CSV-F34       TO        SMS-F292
*        商品分類区分
          MOVE    CSV-F36       TO        SMS-F302
*        管理区分
          MOVE    CSV-F38       TO        SMS-F312
*        定番区分
          MOVE    CSV-F40       TO        SMS-F322
*        Ｄ３６５商品ＣＤ
          MOVE    CSV-F42       TO        SMS-F332
*        Ｄ３６５ＪＡＮＣＤ
          MOVE    CSV-F44       TO        SMS-F342
*2020.05.28↓
*        オーダー区分
          MOVE    CSV-F46       TO        SMS-F352
*2020.05.28↑
     END-IF.
*
* 【更新モード】
     IF   CSV-F02       =   "Y"
*        商品漢字名１
          MOVE    CSV-F05       TO        SMS-F151
          MOVE    CSV-F06       TO        SMS-F152
*        商品漢字名２
          MOVE    CSV-F07       TO        SMS-F161
          MOVE    CSV-F08       TO        SMS-F162
*        商品カナ名１
          MOVE    CSV-F09       TO        SMS-F171
          MOVE    CSV-F10       TO        SMS-F172
*        商品カナ名２
          MOVE    CSV-F11       TO        SMS-F181
          MOVE    CSV-F12       TO        SMS-F182
*        仕入単価
          MOVE    CSV-F13       TO        SMS-F191
*         MOVE    CSV-F14       TO        SMS-F192
          COMPUTE SMS-F192  =   CSV-F14 / 100
*        基準原価
          MOVE    CSV-F15       TO        SMS-F201
*         MOVE    CSV-F16       TO        SMS-F202
          COMPUTE SMS-F202  =   CSV-F16 / 100
*        基準売価
          MOVE    CSV-F17       TO        SMS-F211
*         MOVE    CSV-F18       TO        SMS-F212
          COMPUTE SMS-F212  =   CSV-F18 / 100
*        主仕入先コード
          MOVE    CSV-F19       TO        SMS-F221
          MOVE    CSV-F20       TO        SMS-F222
*        ＪＡＮコード
          MOVE    CSV-F21       TO        SMS-F231
          MOVE    CSV-F22       TO        SMS-F232
*        入数
          MOVE    CSV-F23       TO        SMS-F241
          MOVE    CSV-F24       TO        SMS-F242
*        廃盤区分
          MOVE    CSV-F25       TO        SMS-F251
          MOVE    CSV-F26       TO        SMS-F252
*        サカタ２０分類
          MOVE    CSV-F27       TO        SMS-F261
          MOVE    CSV-F28       TO        SMS-F262
*        小売区分
          MOVE    CSV-F29       TO        SMS-F271
          MOVE    CSV-F30       TO        SMS-F272
*        振替区分
          MOVE    CSV-F31       TO        SMS-F281
          MOVE    CSV-F32       TO        SMS-F282
*        物流_区分
          MOVE    CSV-F33       TO        SMS-F291
          MOVE    CSV-F34       TO        SMS-F292
*        商品分類区分
          MOVE    CSV-F35       TO        SMS-F301
          MOVE    CSV-F36       TO        SMS-F302
*        管理区分
          MOVE    CSV-F37       TO        SMS-F311
          MOVE    CSV-F38       TO        SMS-F312
*        定番区分
          MOVE    CSV-F39       TO        SMS-F321
          MOVE    CSV-F40       TO        SMS-F322
*        Ｄ３６５商品ＣＤ
          MOVE    CSV-F41       TO        SMS-F331
          MOVE    CSV-F42       TO        SMS-F332
*        Ｄ３６５ＪＡＮＣＤ
          MOVE    CSV-F43       TO        SMS-F341
          MOVE    CSV-F44       TO        SMS-F342
*2020.05.28↓
*        オーダー区分
          MOVE    CSV-F45       TO        SMS-F351
          MOVE    CSV-F46       TO        SMS-F352
*2020.05.28↑
     END-IF.
*
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        SMS-F060
     END-IF.
     MOVE     ERR-KBN(01)       TO        SMS-F061.
     MOVE     ERR-KBN(02)       TO        SMS-F062.
     MOVE     ERR-KBN(03)       TO        SMS-F063.
     MOVE     ERR-KBN(04)       TO        SMS-F064.
     MOVE     ERR-KBN(05)       TO        SMS-F065.
     MOVE     ERR-KBN(06)       TO        SMS-F066.
     MOVE     ERR-KBN(07)       TO        SMS-F067.
     MOVE     ERR-KBN(08)       TO        SMS-F068.
     MOVE     ERR-KBN(09)       TO        SMS-F069.
     MOVE     ERR-KBN(10)       TO        SMS-F06A.
*
     WRITE    SMS-REC.
     ADD      1                  TO       SMS-CNT.
*
 WRITE-SMS-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      SMS-CNT      TO    MSG-OUT02.
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
     CLOSE     SMSMNTCS
               SMSMNTW1
               JYOKEN1
               ZSHIMS1
               MEIMS1
               SUBMEIL1
               SUBMEIL7
               SUBMEIL8.
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
