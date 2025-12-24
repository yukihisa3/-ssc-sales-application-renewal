# NVM0140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVM0140B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　商品変換テーブル更新　　　　　　　*
*    作成日／作成者　　　：　2020/04/01 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、商品変換テーブル・　　*
*    　　　　　　　　　　　　サブ商品変換テーブルを更新する　　*
*    流用元　　　　　　　：　STB0020B.TOKSRLIB                 *
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVM0140B.
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
*商品変換テーブル取込ファイル
     SELECT      STBMNTW1    ASSIGN    TO       DA-01-VI-STBMNTW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      STB-F13
                                                STB-F14
                                                STB-F060
                             FILE      STATUS   STB-ST.
*商品変換テーブル
     SELECT      SHOTBL1     ASSIGN    TO       DA-01-VI-SHOTBL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      SHO-F01
                                                SHO-F02
                             FILE      STATUS   SHO-ST.
*サブ商品変換テーブル
     SELECT      SUBTBLL1    ASSIGN    TO       DA-01-VI-SUBTBLL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SUB-F01
                                                SUB-F02
                             FILE      STATUS   SUB-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品変換テーブル取込ファイル
 FD  STBMNTW1.
     COPY        STBMNTW1    OF        XFDLIB
     JOINING     STB         AS        PREFIX.
*商品変換テーブル
 FD  SHOTBL1.
     COPY        SHOTBL1     OF        XFDLIB
     JOINING     SHO         AS        PREFIX.
*サブ商品変換テーブル
 FD  SUBTBLL1.
     COPY        SUBTBLL1    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  STB-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  STB-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  STB-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  SHO-FLG             PIC  X(03)  VALUE  SPACE.
     03  SUB-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHO-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SHO-RT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SUB-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SUB-RT-CNT          PIC  9(07)  VALUE  ZERO.
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
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  STB-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  SUB-ERR             PIC  N(12)  VALUE
                   NC"サブ商品変換テーブル異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVM0140B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVM0140B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"変換テーブル作成＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"変換テーブル更新＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"サブテーブル作成＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"サブテーブル更新＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
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
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD.
****************************************************************
 DECLARATIVES.
 STB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE STBMNTW1.
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
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBTBLL1.
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
     OPEN        INPUT       STBMNTW1.
     OPEN        I-O         SHOTBL1
                             SUBTBLL1.
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
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 変換テーブルＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-STB-SEC.
*
     IF       STB-ENDFLG  =   SPACE
              PERFORM         HENSYU-STB-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  変換テーブルＥＸＣＥＬ　順読込
****************************************************************
 READ-STB-SEC                SECTION.
     MOVE     "READ-STB-SEC"      TO   S-NAME.
*
     READ     STBMNTW1   AT   END
              MOVE      "Y"       TO   STB-ENDFLG
              GO                  TO   READ-STB-EXIT
     END-READ.
     IF       STB-F060  =  "1"
              GO                  TO   READ-STB-SEC
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   STB-RD-CNT.
*
 READ-STB-EXIT.
     EXIT.
***************************************************************
*    作成・更新制御
***************************************************************
 HENSYU-STB-SEC             SECTION.
*
     MOVE    "HENSYU-STB-SEC"      TO   S-NAME.
*
*事前存在チェック
     MOVE     STB-F13      TO      SHO-F01 SUB-F01.
     MOVE     STB-F14      TO      SHO-F02 SUB-F02.
     PERFORM  SHO-READ-SEC.
     PERFORM  SUB-READ-SEC.
*作成・更新制御
*　　登録モード
     IF  STB-F11  =  "1"
*　　    商品変換ＴＢＬ非存在→登録
         IF   SHO-FLG  =  "INV"
              PERFORM  SHO-NEW-SEC
*　　    商品変換ＴＢＬ　存在→更新
         ELSE
              PERFORM  SHO-UPD2-SEC
         END-IF
*　　    サブ商品変換ＴＢＬ非存在→登録
         IF   SUB-FLG  =  "INV"
              PERFORM  SUB-NEW-SEC
*　　    サブ商品変換ＴＢＬ　存在→更新
         ELSE
              PERFORM  SUB-UPD2-SEC
         END-IF
*　　更新モード
     ELSE
*　　    どちらにも非存在→エラー（アベンド）
*        　先行処理にてチェック済であるため
*        　当条件はヒットしないが
*      　  ロジックとしては組み込んでおく
         IF ( SHO-FLG  =  "INV" ) AND ( SUB-FLG  =  "INV" )
              DISPLAY NC"商品変換テーブル存在しません" UPON CONS
              DISPLAY NC"　取引先ＣＤ＝" STB-F13       UPON CONS
              DISPLAY NC"　量販店商品＝" STB-F14       UPON CONS
              MOVE    4000        TO        PROGRAM-STATUS
              STOP    RUN
         END-IF
*
*　　    商品変換ＴＢＬ非存在→サブから登録
         IF   SHO-FLG  =  "INV"
              PERFORM  SHO-NEW2-SEC
*　　    商品変換ＴＢＬ　存在→更新
         ELSE
              PERFORM  SHO-UPD-SEC
         END-IF
*　　    サブ商品変換ＴＢＬ非存在→ＴＢＬから登録
         IF   SUB-FLG  =  "INV"
              PERFORM  SUB-NEW2-SEC
*　　    サブ商品変換ＴＢＬ　存在→更新
         ELSE
              PERFORM  SUB-UPD-SEC
         END-IF
     END-IF.
*
 HENSYU-STB-EXIT.
     EXIT.
****************************************************************
*   商品変換テーブル作成（登録モードでレコード非存在時）
*   　　　　　　　　　　　取込ファイルより作成
****************************************************************
 SHO-NEW-SEC        SECTION.
*
     MOVE "SHO-NEW-SEC"      TO   S-NAME.
     INITIALIZE                   SHO-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SHO-F02.
*
*   商品コード
     MOVE  STB-F152          TO   SHO-F031.
*
*   品単１
     MOVE  STB-F162          TO   SHO-F0321.
*
*   品単２
     MOVE  STB-F172          TO   SHO-F0322.
*
*   品単３
     MOVE  STB-F182          TO   SHO-F0323.
*
*   出荷場所
     MOVE  STB-F192          TO   SHO-F04.
*
*   原価単価
     MOVE  STB-F202          TO   SHO-F05.
*
*   売価単価
     MOVE  STB-F212          TO   SHO-F06.
*
*   分類コード
     MOVE  STB-F222          TO   SHO-F07.
*
*   棚番
     MOVE  STB-F232          TO   SHO-F08.
*
*   仕入単価
     MOVE  STB-F242          TO   SHO-F09.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  STB-F252          TO   SHO-F11.
*
*   出荷地域区分
*    F12:INIT
*
*------------------------------------------------------------
*
*   登録担当者
     MOVE  STB-F04           TO   SHO-F13.
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
     MOVE  STB-F01           TO   SHO-F98.
*
*   更新日
*    F99:INIT
*
*------------------------------------------------------------
*
*   商品変換テーブルＷＲＩＴＥ
     WRITE      SHO-REC.
     ADD        1            TO   SHO-WT-CNT.
*
 SHO-NEW-EXIT.
     EXIT.
****************************************************************
*   商品変換テーブル作成（更新モードでレコード非存在時）
*   　　　　　　　　　　　サブから作成
****************************************************************
 SHO-NEW2-SEC        SECTION.
*
     MOVE "SHO-NEW2-SEC"     TO   S-NAME.
     INITIALIZE                   SHO-REC.
*
*   取引先コード
     MOVE  SUB-F01           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  SUB-F02           TO   SHO-F02.
*
*   商品コード
     MOVE  SUB-F031          TO   SHO-F031.
     IF    STB-F151  =  "Y"
           MOVE  STB-F152    TO   SHO-F031
     END-IF.
*
*   品単１
     MOVE  SUB-F0321         TO   SHO-F0321.
     IF    STB-F161  =  "Y"
           MOVE  STB-F162    TO   SHO-F0321
     END-IF.
*
*   品単２
     MOVE  SUB-F0322         TO   SHO-F0322.
     IF    STB-F171  =  "Y"
           MOVE  STB-F172    TO   SHO-F0322
     END-IF.
*
*   品単３
     MOVE  SUB-F0323         TO   SHO-F0323.
     IF    STB-F181  =  "Y"
           MOVE  STB-F182    TO   SHO-F0323
     END-IF.
*
*   出荷場所
     MOVE  SUB-F04           TO   SHO-F04.
     IF    STB-F191  =  "Y"
           MOVE  STB-F192    TO   SHO-F04
     END-IF.
*
*   原価単価
     MOVE  SUB-F05           TO   SHO-F05.
     IF    STB-F201  =  "Y"
           MOVE  STB-F202    TO   SHO-F05
     END-IF.
*
*   売価単価
     MOVE  SUB-F06           TO   SHO-F06.
     IF    STB-F211  =  "Y"
           MOVE  STB-F212    TO   SHO-F06
     END-IF.
*
*   分類コード
     MOVE  SUB-F07           TO   SHO-F07.
     IF    STB-F221  =  "Y"
           MOVE  STB-F222    TO   SHO-F07
     END-IF.
*
*   棚番
     MOVE  SUB-F08           TO   SHO-F08.
     IF    STB-F231  =  "Y"
           MOVE  STB-F232    TO   SHO-F08
     END-IF.
*
*   仕入単価
     MOVE  SUB-F09           TO   SHO-F09.
     IF    STB-F241  =  "Y"
           MOVE  STB-F242    TO   SHO-F09
     END-IF.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  SUB-F11           TO   SHO-F11.
     IF    STB-F251  =  "Y"
           MOVE  STB-F252    TO   SHO-F11
     END-IF.
*
*   出荷地域区分
*    F12:INIT
*
*------------------------------------------------------------
*
*   登録担当者
     MOVE  STB-F04           TO   SHO-F13.
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
     MOVE  STB-F01           TO   SHO-F98.
*
*   更新日
     MOVE  STB-F01           TO   SHO-F99.
*
*------------------------------------------------------------
*
*   商品変換テーブルＷＲＩＴＥ
     WRITE      SHO-REC.
     ADD        1            TO   SHO-WT-CNT.
*
 SHO-NEW2-EXIT.
     EXIT.
****************************************************************
*   サブ商品変換テーブル作成（登録モードでレコード非存在時）
*   　　　　　　　　　　　　　取込ファイルより作成
****************************************************************
 SUB-NEW-SEC        SECTION.
*
     MOVE "SUB-NEW-SEC"      TO   S-NAME.
     INITIALIZE                   SUB-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SUB-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SUB-F02.
*
*   商品コード
     MOVE  STB-F152          TO   SUB-F031.
*
*   品単１
     MOVE  STB-F162          TO   SUB-F0321.
*
*   品単２
     MOVE  STB-F172          TO   SUB-F0322.
*
*   品単３
     MOVE  STB-F182          TO   SUB-F0323.
*
*   出荷場所
     MOVE  STB-F192          TO   SUB-F04.
*
*   原価単価
     MOVE  STB-F202          TO   SUB-F05.
*
*   売価単価
     MOVE  STB-F212          TO   SUB-F06.
*
*   分類コード
     MOVE  STB-F222          TO   SUB-F07.
*
*   棚番
     MOVE  STB-F232          TO   SUB-F08.
*
*   仕入単価
     MOVE  STB-F242          TO   SUB-F09.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  STB-F252          TO   SUB-F11.
*
*   出荷地域区分
*    F12:INIT
*
*------------------------------------------------------------
*
*   予備ＪＡＮＣＤ
     MOVE  STB-F262          TO   SUB-F13.
*
*   予備インストアＣＤ
     MOVE  STB-F272          TO   SUB-F14.
*
*   入数
     MOVE  STB-F282          TO   SUB-F15.
*
*   振分倉庫
     MOVE  STB-F292          TO   SUB-F16.
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  STB-F55           TO   SUB-F17.
*
*   Ｄ３６５商品ＣＤ
     MOVE  STB-F56           TO   SUB-F18.
*
*   直送/セット組区分
     MOVE  STB-F302          TO   SUB-F19.
*
*   登録担当者部門
     MOVE  STB-F03           TO   SUB-F92.
*
*   登録担当者ＣＤ
     MOVE  STB-F04           TO   SUB-F93.
*
*   登録日付
     MOVE  STB-F01           TO   SUB-F94.
*
*   登録時刻
     MOVE  STB-F02           TO   SUB-F95.
*
*   更新担当者部門
*    F96:INIT
*
*   更新担当者ＣＤ
*    F97:INIT
*
*   更新日付
*    F98:INIT
*
*   更新時刻
*    F99:INIT
*
*------------------------------------------------------------
*
*   サブ商品変換テーブルＷＲＩＴＥ
     WRITE      SUB-REC.
     ADD        1            TO   SUB-WT-CNT.
*
 SUB-NEW-EXIT.
     EXIT.
****************************************************************
*   サブ商品変換テーブル作成（更新モードでレコード非存在時）
*   　　　　　　　　　　　　　商品変換テーブルから作成
****************************************************************
 SUB-NEW2-SEC        SECTION.
*
     MOVE "SUB-NEW2-SEC"     TO   S-NAME.
     INITIALIZE                   SUB-REC.
*
*   取引先コード
     MOVE  SHO-F01           TO   SUB-F01.
*
*   量販店商品コード
     MOVE  SHO-F02           TO   SUB-F02.
*
*   商品コード
     MOVE  SHO-F031          TO   SUB-F031.
     IF    STB-F151  =  "Y"
           MOVE  STB-F152    TO   SUB-F031
     END-IF.
*
*   品単１
     MOVE  SHO-F0321         TO   SUB-F0321.
     IF    STB-F161  =  "Y"
           MOVE  STB-F162    TO   SUB-F0321
     END-IF.
*
*   品単２
     MOVE  SHO-F0322         TO   SUB-F0322.
     IF    STB-F171  =  "Y"
           MOVE  STB-F172    TO   SUB-F0322
     END-IF.
*
*   品単３
     MOVE  SHO-F0323         TO   SUB-F0323.
     IF    STB-F181  =  "Y"
           MOVE  STB-F182    TO   SUB-F0323
     END-IF.
*
*   出荷場所
     MOVE  SHO-F04           TO   SUB-F04.
     IF    STB-F191  =  "Y"
           MOVE  STB-F192    TO   SUB-F04
     END-IF.
*
*   原価単価
     MOVE  SHO-F05           TO   SUB-F05.
     IF    STB-F201  =  "Y"
           MOVE  STB-F202    TO   SUB-F05
     END-IF.
*
*   売価単価
     MOVE  SHO-F06           TO   SUB-F06.
     IF    STB-F211  =  "Y"
           MOVE  STB-F212    TO   SUB-F06
     END-IF.
*
*   分類コード
     MOVE  SHO-F07           TO   SUB-F07.
     IF    STB-F221  =  "Y"
           MOVE  STB-F222    TO   SUB-F07
     END-IF.
*
*   棚番
     MOVE  SHO-F08           TO   SUB-F08.
     IF    STB-F231  =  "Y"
           MOVE  STB-F232    TO   SUB-F08
     END-IF.
*
*   仕入単価
     MOVE  SHO-F09           TO   SUB-F09.
     IF    STB-F241  =  "Y"
           MOVE  STB-F242    TO   SUB-F09
     END-IF.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  SHO-F11           TO   SUB-F11.
     IF    STB-F251  =  "Y"
           MOVE  STB-F252    TO   SUB-F11
     END-IF.
*
*   出荷地域区分
*    F12:INIT
*
*------------------------------------------------------------
*
*   予備ＪＡＮＣＤ
     MOVE  SPACE             TO   SUB-F13.
     IF    STB-F261  =  "Y"
           MOVE  STB-F262    TO   SUB-F13
     END-IF.
*
*   予備インストアＣＤ
     MOVE  SPACE             TO   SUB-F14.
     IF    STB-F271  =  "Y"
           MOVE  STB-F272    TO   SUB-F14
     END-IF.
*
*   入数
     MOVE  ZERO              TO   SUB-F15.
     IF    STB-F281  =  "Y"
           MOVE  STB-F282    TO   SUB-F15
     END-IF.
*
*   振分倉庫
     MOVE  SPACE             TO   SUB-F16.
     IF    STB-F291  =  "Y"
           MOVE  STB-F292    TO   SUB-F16
     END-IF.
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  STB-F55           TO   SUB-F17.
*
*   Ｄ３６５商品ＣＤ
     MOVE  STB-F56           TO   SUB-F18.
*
*   直送/セット組区分
     MOVE  SPACE             TO   SUB-F19.
     IF    STB-F301  =  "Y"
           MOVE  STB-F302    TO   SUB-F19
     END-IF.
*
*   登録担当者部門
     MOVE  STB-F03           TO   SUB-F92.
*
*   登録担当者ＣＤ
     MOVE  STB-F04           TO   SUB-F93.
*
*   登録日付
     MOVE  STB-F01           TO   SUB-F94.
*
*   登録時刻
     MOVE  STB-F02           TO   SUB-F95.
*
*   更新担当者部門
     MOVE  STB-F03           TO   SUB-F96.
*
*   更新担当者ＣＤ
     MOVE  STB-F04           TO   SUB-F97.
*
*   更新日付
     MOVE  STB-F01           TO   SUB-F98.
*
*   更新時刻
     MOVE  STB-F02           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   サブ商品変換テーブルＷＲＩＴＥ
     WRITE      SUB-REC.
     ADD        1            TO   SUB-WT-CNT.
*
 SUB-NEW2-EXIT.
     EXIT.
****************************************************************
*   商品変換テーブル更新（更新モードでレコード存在時）
*   　　　　　　　　　　　取込ファイルより更新
****************************************************************
 SHO-UPD-SEC        SECTION.
*
     MOVE "SHO-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SHO-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SHO-F02.
*
*   商品コード
     IF    STB-F151  =  "Y"
           MOVE  STB-F152    TO   SHO-F031
     END-IF.
*
*   品単１
     IF    STB-F161  =  "Y"
           MOVE  STB-F162    TO   SHO-F0321
     END-IF.
*
*   品単２
     IF    STB-F171  =  "Y"
           MOVE  STB-F172    TO   SHO-F0322
     END-IF.
*   品単３
     IF    STB-F181  =  "Y"
           MOVE  STB-F182    TO   SHO-F0323
     END-IF.
*
*   出荷場所
     IF    STB-F191  =  "Y"
           MOVE  STB-F192    TO   SHO-F04
     END-IF.
*
*   原価単価
     IF    STB-F201  =  "Y"
           MOVE  STB-F202    TO   SHO-F05
     END-IF.
*
*   売価単価
     IF    STB-F211  =  "Y"
           MOVE  STB-F212    TO   SHO-F06
     END-IF.
*
*   分類コード
     IF    STB-F221  =  "Y"
           MOVE  STB-F222    TO   SHO-F07
     END-IF.
*
*   棚番
     IF    STB-F231  =  "Y"
           MOVE  STB-F232    TO   SHO-F08
     END-IF.
*
*   仕入単価
     IF    STB-F241  =  "Y"
           MOVE  STB-F242    TO   SHO-F09
     END-IF.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     IF    STB-F251  =  "Y"
           MOVE  STB-F252    TO   SHO-F11
     END-IF.
*
*   出荷地域区分
*    F12:INIT
*
*   登録担当者
*    F13:-
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
*    F98:-
*
*   更新日
     MOVE  STB-F01           TO   SHO-F99.
*
*------------------------------------------------------------
*
*   商品変換テーブルＲＥＷＲＩＴＥ
     REWRITE    SHO-REC.
     ADD        1            TO   SHO-RT-CNT.
*
 SHO-UPD-EXIT.
     EXIT.
*
****************************************************************
*   商品変換テーブル更新（登録モードでレコード存在時）
*   　　　　　　　　　　　取込ファイルより全項目置き換え
****************************************************************
 SHO-UPD2-SEC        SECTION.
*
     MOVE "SHO-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SHO-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SHO-F02.
*
*   商品コード
     MOVE  STB-F152    TO   SHO-F031.
*
*   品単１
     MOVE  STB-F162    TO   SHO-F0321.
*
*   品単２
     MOVE  STB-F172    TO   SHO-F0322.
*
*   品単３
     MOVE  STB-F182    TO   SHO-F0323.
*
*   出荷場所
     MOVE  STB-F192    TO   SHO-F04.
*
*   原価単価
     MOVE  STB-F202    TO   SHO-F05.
*
*   売価単価
     MOVE  STB-F212    TO   SHO-F06.
*
*   分類コード
     MOVE  STB-F222    TO   SHO-F07.
*
*   棚番
     MOVE  STB-F232    TO   SHO-F08.
*
*   仕入単価
     MOVE  STB-F242    TO   SHO-F09.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  STB-F252    TO   SHO-F11.
*
*   出荷地域区分
*    F12:INIT
*
*   登録担当者
*    F13:-
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
*    F98:-
*
*   更新日
     MOVE  STB-F01           TO   SHO-F99.
*
*------------------------------------------------------------
*
*   商品変換テーブルＲＥＷＲＩＴＥ
     REWRITE    SHO-REC.
     ADD        1            TO   SHO-RT-CNT.
*
 SHO-UPD2-EXIT.
     EXIT.
*
****************************************************************
*   サブ商品変換テーブル更新（更新モードでレコード存在時）
*   　　　　　　　　　　　　　取込ファイルより更新
****************************************************************
 SUB-UPD-SEC        SECTION.
*
     MOVE "SUB-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SUB-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SUB-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SUB-F02.
*
*   商品コード
     IF    STB-F151  =  "Y"
           MOVE  STB-F152    TO   SUB-F031
     END-IF.
*
*   品単１
     IF    STB-F161  =  "Y"
           MOVE  STB-F162    TO   SUB-F0321
     END-IF.
*
*   品単２
     IF    STB-F171  =  "Y"
           MOVE  STB-F172    TO   SUB-F0322
     END-IF.
*   品単３
     IF    STB-F181  =  "Y"
           MOVE  STB-F182    TO   SUB-F0323
     END-IF.
*
*   出荷場所
     IF    STB-F191  =  "Y"
           MOVE  STB-F192    TO   SUB-F04
     END-IF.
*
*   原価単価
     IF    STB-F201  =  "Y"
           MOVE  STB-F202    TO   SUB-F05
     END-IF.
*
*   売価単価
     IF    STB-F211  =  "Y"
           MOVE  STB-F212    TO   SUB-F06
     END-IF.
*
*   分類コード
     IF    STB-F221  =  "Y"
           MOVE  STB-F222    TO   SUB-F07
     END-IF.
*
*   棚番
     IF    STB-F231  =  "Y"
           MOVE  STB-F232    TO   SUB-F08
     END-IF.
*
*   仕入単価
     IF    STB-F241  =  "Y"
           MOVE  STB-F242    TO   SUB-F09
     END-IF.
*
*   ﾗﾍﾞﾙ張替区分
*    非更新
*
*   検品Ｇ
     IF    STB-F251  =  "Y"
           MOVE  STB-F252    TO   SUB-F11
     END-IF.
*
*   出荷地域区分
*    非更新
*
*------------------------------------------------------------
*
*   予備ＪＡＮＣＤ
     IF    STB-F261  =  "Y"
           MOVE  STB-F262    TO   SUB-F13
     END-IF.
*
*   予備インストアＣＤ
     IF    STB-F271  =  "Y"
           MOVE   STB-F272   TO   SUB-F14
     END-IF.
*
*   入数
     IF    STB-F281  =  "Y"
            MOVE  STB-F282   TO   SUB-F15
     END-IF.
*
*   振分倉庫
     IF    STB-F291  =  "Y"
           MOVE   STB-F292   TO   SUB-F16
     END-IF.
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  STB-F55           TO   SUB-F17.
*
*   Ｄ３６５商品ＣＤ
     MOVE  STB-F56           TO   SUB-F18.
*
*   直送/セット組区分
     IF    STB-F301  =  "Y"
           MOVE  STB-F302    TO   SUB-F19
     END-IF.
*
*   登録担当者部門
*    非更新
*
*   登録担当者ＣＤ
*    非更新
*
*   登録日付
*    非更新
*
*   登録時刻
*    非更新
*
*   更新担当者部門
     MOVE  STB-F03           TO   SUB-F96.
*
*   更新担当者ＣＤ
     MOVE  STB-F04           TO   SUB-F97.
*
*   更新日付
     MOVE  STB-F01           TO   SUB-F98.
*
*   更新時刻
     MOVE  STB-F02           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   サブ商品変換テーブルＲＥＷＲＩＴＥ
     REWRITE    SUB-REC.
     ADD        1            TO   SUB-RT-CNT.
*
 SUB-UPD-EXIT.
     EXIT.
*
****************************************************************
*   サブ商品変換テーブル更新（登録モードでレコード存在時）
*   　　　　　　　　　　　　　取込ファイルより全項目置き換え
****************************************************************
 SUB-UPD2-SEC        SECTION.
*
     MOVE "SUB-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SUB-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SUB-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SUB-F02.
*
*   商品コード
     MOVE  STB-F152          TO   SUB-F031.
*
*   品単１
     MOVE  STB-F162          TO   SUB-F0321.
*
*   品単２
     MOVE  STB-F172          TO   SUB-F0322.
*   品単３
     MOVE  STB-F182          TO   SUB-F0323.
*
*   出荷場所
     MOVE  STB-F192          TO   SUB-F04.
*
*   原価単価
     MOVE  STB-F202          TO   SUB-F05.
*
*   売価単価
     MOVE  STB-F212          TO   SUB-F06.
*
*   分類コード
     MOVE  STB-F222          TO   SUB-F07.
*
*   棚番
     MOVE  STB-F232          TO   SUB-F08.
*
*   仕入単価
     MOVE  STB-F242          TO   SUB-F09.
*
*   ﾗﾍﾞﾙ張替区分
*    非更新
*
*   検品Ｇ
     MOVE  STB-F252          TO   SUB-F11.
*
*   出荷地域区分
*    非更新
*
*------------------------------------------------------------
*
*   予備ＪＡＮＣＤ
     MOVE  STB-F262          TO   SUB-F13.
*
*   予備インストアＣＤ
     MOVE  STB-F272          TO   SUB-F14.
*
*   入数
     MOVE  STB-F282          TO   SUB-F15.
*
*   振分倉庫
     MOVE  STB-F292          TO   SUB-F16.
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  STB-F55           TO   SUB-F17.
*
*   Ｄ３６５商品ＣＤ
     MOVE  STB-F56           TO   SUB-F18.
*
*   直送/セット組区分
     MOVE  STB-F302    TO   SUB-F19.
*
*   登録担当者部門
*    非更新
*
*   登録担当者ＣＤ
*    非更新
*
*   登録日付
*    非更新
*
*   登録時刻
*    非更新
*
*   更新担当者部門
     MOVE  STB-F03           TO   SUB-F96.
*
*   更新担当者ＣＤ
     MOVE  STB-F04           TO   SUB-F97.
*
*   更新日付
     MOVE  STB-F01           TO   SUB-F98.
*
*   更新時刻
     MOVE  STB-F02           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   サブ商品変換テーブルＲＥＷＲＩＴＥ
     REWRITE    SUB-REC.
     ADD        1            TO   SUB-RT-CNT.
*
 SUB-UPD2-EXIT.
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
              MOVE "INV"     TO   SHO-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHO-FLG
     END-READ.
 SHO-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品変換ＴＢＬ検索
****************************************************************
 SUB-READ-SEC               SECTION.
     MOVE     "SUB-READ-SEC" TO   S-NAME.
*
     READ     SUBTBLL1
       INVALID
              MOVE "INV"     TO   SUB-FLG
       NOT  INVALID
              MOVE SPACE     TO   SUB-FLG
     END-READ.
 SUB-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      STB-RD-CNT   TO    MSG-OUT01.
     MOVE      SHO-WT-CNT   TO    MSG-OUT02.
     MOVE      SHO-RT-CNT   TO    MSG-OUT03.
     MOVE      SUB-WT-CNT   TO    MSG-OUT04.
     MOVE      SUB-RT-CNT   TO    MSG-OUT05.
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
*
     CLOSE     STBMNTW1
               SHOTBL1
               SUBTBLL1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
