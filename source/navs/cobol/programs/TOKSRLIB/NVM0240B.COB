****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　商品名称マスタ　更新　　　　　　　*
*    作成日／作成者　　　：　2020/04/16 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、商品名称マスタ　・　　*
*    　　　　　　　　　　　　サブ商品名称マスタ　を更新する　　*
*    流用元　　　　　　　：　NVM0140B.TOKSRLIB                 *
*    変更日／変更者　　　：　2020/05/28 INOUE                  *
*    変更内容　　　　　　：　オーダー区分追加                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVM0240B.
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
*商品名称マスタ取込ファイル
     SELECT      SMSMNTW1    ASSIGN    TO       DA-01-VI-SMSMNTW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      SMS-F13
                                                SMS-F14
                                                SMS-F060
                             FILE      STATUS   SMS-ST.
*商品名称マスタ　
     SELECT      MEIMS1      ASSIGN    TO       DA-01-VI-MEIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
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
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品名称マスタ取込ファイル
 FD  SMSMNTW1.
     COPY        SMSMNTW1    OF        XFDLIB
     JOINING     SMS         AS        PREFIX.
*商品名称マスタ　
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*サブ商品名称マスタ　
 FD  SUBMEIL1.
     COPY        SUBMEIL1    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  SMS-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SMS-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  SMS-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEI-FLG             PIC  X(03)  VALUE  SPACE.
     03  SUB-FLG             PIC  X(03)  VALUE  SPACE.
     03  MEI-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEI-RT-CNT          PIC  9(07)  VALUE  ZERO.
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
     03  SMS-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  MEI-ERR             PIC  N(09)  VALUE
                   NC"商品名称マスタ異常".
     03  SUB-ERR             PIC  N(11)  VALUE
                   NC"サブ商品名称マスタ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVM0240B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVM0240B".
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
                             NC"名称マスタ　作成＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"名称マスタ　更新＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"サブマスタ　作成＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"サブマスタ　更新＝".
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
     OPEN        INPUT       SMSMNTW1.
     OPEN        I-O         MEIMS1
                             SUBMEIL1.
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
* 名称マスタＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-SMS-SEC.
*
     IF       SMS-ENDFLG  =   SPACE
              PERFORM         HENSYU-SMS-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  商品名称マスタ　ＥＸＣＥＬ　順読込
****************************************************************
 READ-SMS-SEC                SECTION.
     MOVE     "READ-SMS-SEC"      TO   S-NAME.
*
     READ     SMSMNTW1   AT   END
              MOVE      "Y"       TO   SMS-ENDFLG
              GO                  TO   READ-SMS-EXIT
     END-READ.
     IF       SMS-F060  =  "1"
              GO                  TO   READ-SMS-SEC
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   SMS-RD-CNT.
*
 READ-SMS-EXIT.
     EXIT.
***************************************************************
*    作成・更新制御
***************************************************************
 HENSYU-SMS-SEC             SECTION.
*
     MOVE    "HENSYU-SMS-SEC"      TO   S-NAME.
*
*事前存在チェック
*  商品ＣＤ
     MOVE     SMS-F13      TO      MEI-F011 SUB-F011.
*  品単ＣＤ
     MOVE     SMS-F14      TO      MEI-F012 SUB-F012.
*
*  名称マスタ
     PERFORM  MEI-READ-SEC.
*  サブマスタ
     PERFORM  SUB-READ-SEC.
*
*作成・更新制御
*　　登録モード
     IF  SMS-F11  =  "1"
*　　    商品名称マスタ非存在→登録
         IF   MEI-FLG  =  "INV"
              PERFORM  MEI-NEW-SEC
*　　    商品名称マスタ　存在→更新
         ELSE
              PERFORM  MEI-UPD2-SEC
         END-IF
*　　    サブ商品名称マスタ非存在→登録
         IF   SUB-FLG  =  "INV"
              PERFORM  SUB-NEW-SEC
*　　    サブ商品名称マスタ　存在→更新
         ELSE
              PERFORM  SUB-UPD2-SEC
         END-IF
*　　更新モード
     ELSE
*　　    どちらにも非存在→エラー（アベンド）
*        　先行処理にてチェック済であるため
*        　当条件はヒットしないが
*      　  ロジックとしては組み込んでおく
         IF ( MEI-FLG  =  "INV" ) AND ( SUB-FLG  =  "INV" )
              DISPLAY NC"商品名称マスタ　存在しません" UPON CONS
              DISPLAY NC"　商品コード＝" SMS-F13       UPON CONS
              DISPLAY NC"　品単コード＝" SMS-F14       UPON CONS
              MOVE    4000        TO        PROGRAM-STATUS
              STOP    RUN
         END-IF
*
*　　    商品名称マスタ非存在→サブから登録
         IF   MEI-FLG  =  "INV"
              PERFORM  MEI-NEW2-SEC
*　　    商品名称マスタ　存在→更新
         ELSE
              PERFORM  MEI-UPD-SEC
         END-IF
*　　    サブ商品名称マスタ非存在→マスタから登録
         IF   SUB-FLG  =  "INV"
              PERFORM  SUB-NEW2-SEC
*　　    サブ商品名称マスタ　存在→更新
         ELSE
              PERFORM  SUB-UPD-SEC
         END-IF
     END-IF.
*
 HENSYU-SMS-EXIT.
     EXIT.
****************************************************************
*A  商品名称マスタ　作成（登録モードでレコード非存在時）
*   　　　　　　　　　　　取込ファイルより作成
****************************************************************
 MEI-NEW-SEC        SECTION.
*
     MOVE "MEI-NEW-SEC"      TO   S-NAME.
     INITIALIZE                   MEI-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   MEI-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   MEI-F012.
*
*   商品名漢字１
     MOVE  SMS-F152          TO   MEI-F021.
*
*   商品名漢字２
     MOVE  SMS-F162          TO   MEI-F022.
*
*   商品カナ名１
     MOVE  SMS-F172          TO   MEI-F031.
*
*   商品カナ名２
     MOVE  SMS-F182          TO   MEI-F032.
*
*   入数
     MOVE  SMS-F192          TO   MEI-F041.
*
*   基準原価
     MOVE  SMS-F202          TO   MEI-F042.
*
*   基準売価
     MOVE  SMS-F212          TO   MEI-F043.
*
*   主仕入先コード
     MOVE  SMS-F222          TO   MEI-F05.
*
*   ＪＡＮコード
     MOVE  SMS-F232          TO   MEI-F06.
*
*   入数
     MOVE  SMS-F242          TO   MEI-F07.
* ----------------------------------------
*   廃盤区分
     MOVE  SMS-F252          TO   MEI-F08.
*
*   サカタ２０分類
     MOVE  SMS-F262          TO   MEI-F09.
*
*   小売区分
     MOVE  SMS-F272          TO   MEI-F10.
*
*   振替区分
     MOVE  SMS-F282          TO   MEI-FIL1.
*
*   物流束区分
     MOVE  SMS-F292          TO   MEI-F89.
*
*   商品分類区分
     MOVE  SMS-F302          TO   MEI-F90.
*
*   管理区分
     MOVE  SMS-F312          TO   MEI-F91.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  SMS-F322          TO   MEI-F95.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*------------------------------------------------------------
*
*   登録日
     MOVE  SMS-F01           TO   MEI-F98.
*
*   更新日
*    F99:INIT
*
*------------------------------------------------------------
*
*   商品名称マスタ　ＷＲＩＴＥ
     WRITE      MEI-REC.
     ADD        1            TO   MEI-WT-CNT.
*
 MEI-NEW-EXIT.
     EXIT.
****************************************************************
*B  商品名称マスタ　作成（更新モードでレコード非存在時）
*   　　　　　　　　　　　サブから作成
****************************************************************
 MEI-NEW2-SEC        SECTION.
*
     MOVE "MEI-NEW2-SEC"     TO   S-NAME.
     INITIALIZE                   MEI-REC.
*
*   商品コード
     MOVE  SUB-F011          TO   MEI-F011.
*
*   品単コード
     MOVE  SUB-F012          TO   MEI-F012.
*
*   商品名漢字１
     MOVE  SUB-F021          TO   MEI-F021.
     IF    SMS-F151  =  "Y"
           MOVE  SMS-F152    TO   MEI-F021
     END-IF.
*
*   商品名漢字２
     MOVE  SUB-F022          TO   MEI-F022.
     IF    SMS-F161  =  "Y"
           MOVE  SMS-F162    TO   MEI-F022
     END-IF.
*
*   商品カナ名１
     MOVE  SUB-F031          TO   MEI-F031.
     IF    SMS-F171  =  "Y"
           MOVE  SMS-F172    TO   MEI-F031
     END-IF.
*
*   商品カナ名２
     MOVE  SUB-F032          TO   MEI-F032.
     IF    SMS-F181  =  "Y"
           MOVE  SMS-F182    TO   MEI-F032
     END-IF.
*
*   仕入単価
     MOVE  SUB-F041          TO   MEI-F041.
     IF    SMS-F191  =  "Y"
           MOVE  SMS-F192    TO   MEI-F041
     END-IF.
*
*   基準原価
     MOVE  SUB-F042          TO   MEI-F042.
     IF    SMS-F201  =  "Y"
           MOVE  SMS-F202    TO   MEI-F042
     END-IF.
*
*   基準売価
     MOVE  SUB-F043           TO   MEI-F043.
     IF    SMS-F211  =  "Y"
           MOVE  SMS-F212    TO   MEI-F043
     END-IF.
*
*   主仕入先コード
     MOVE  SUB-F05           TO   MEI-F05.
     IF    SMS-F221  =  "Y"
           MOVE  SMS-F222    TO   MEI-F05
     END-IF.
*
*   ＪＡＮコード
     MOVE  SUB-F06           TO   MEI-F06.
     IF    SMS-F231  =  "Y"
           MOVE  SMS-F232    TO   MEI-F06
     END-IF.
*
*   入数
     MOVE  SUB-F07           TO   MEI-F07.
     IF    SMS-F241  =  "Y"
           MOVE  SMS-F242    TO   MEI-F07
     END-IF.
* -----------------------------------------
*   廃盤区分
     MOVE  SUB-F08           TO   MEI-F08.
     IF    SMS-F251  =  "Y"
           MOVE  SMS-F252    TO   MEI-F08
     END-IF.
*
*   サカタ２０分類
     MOVE  SUB-F09           TO   MEI-F09.
     IF    SMS-F261  =  "Y"
           MOVE  SMS-F262    TO   MEI-F09
     END-IF.
*
*   小売区分
     MOVE  SUB-F10           TO   MEI-F10.
     IF    SMS-F271  =  "Y"
           MOVE  SMS-F272    TO   MEI-F10
     END-IF.
*
*   振替区分
     MOVE  SUB-FIL1          TO   MEI-FIL1.
     IF    SMS-F281  =  "Y"
           MOVE  SMS-F282    TO   MEI-FIL1
     END-IF.
*
*   物流束区分
     MOVE  SUB-F89           TO   MEI-F89.
     IF    SMS-F291  =  "Y"
           MOVE  SMS-F292    TO   MEI-F89
     END-IF.
*
*   商品分類区分
     MOVE  SUB-F90           TO   MEI-F90.
     IF    SMS-F301  =  "Y"
           MOVE  SMS-F302    TO   MEI-F90
     END-IF.
*
*   管理区分
     MOVE  SUB-F91           TO   MEI-F91.
     IF    SMS-F311  =  "Y"
           MOVE  SMS-F312    TO   MEI-F91
     END-IF.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  SUB-F95           TO   MEI-F95.
     IF    SMS-F321  =  "Y"
           MOVE  SMS-F322    TO   MEI-F95
     END-IF.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*------------------------------------------------------------
*
*   登録日
     MOVE  SUB-F98           TO   MEI-F98.
*
*   更新日
     MOVE  SMS-F01           TO   MEI-F99.
*
*------------------------------------------------------------
*
*   商品名称マスタ　ＷＲＩＴＥ
     WRITE      MEI-REC.
     ADD        1            TO   MEI-WT-CNT.
*
 MEI-NEW2-EXIT.
     EXIT.
****************************************************************
*A  サブ商品名称マスタ　作成（登録モードでレコード非存在時）
*   　　　　　　　　　　　　　取込ファイルより作成
****************************************************************
 SUB-NEW-SEC        SECTION.
*
     MOVE "SUB-NEW-SEC"      TO   S-NAME.
     INITIALIZE                   SUB-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   SUB-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   SUB-F012.
*
*   商品漢字名１
     MOVE  SMS-F152          TO   SUB-F021.
*
*   商品名漢字２
     MOVE  SMS-F162          TO   SUB-F022.
*
*   商品カナ名１
     MOVE  SMS-F172          TO   SUB-F031.
*
*   商品カナ名２
     MOVE  SMS-F182          TO   SUB-F032.
*
*   仕入単価
     MOVE  SMS-F192          TO   SUB-F041.
*
*   基準原価
     MOVE  SMS-F202          TO   SUB-F042.
*
*   基準売価
     MOVE  SMS-F212          TO   SUB-F043.
*
*   主仕入先コード
     MOVE  SMS-F222          TO   SUB-F05.
*
*   ＪＡＮコード
     MOVE  SMS-F232          TO   SUB-F06.
*
*   入数
     MOVE  SMS-F242          TO   SUB-F07.
*
* ----------------------------------------
*   廃盤区分
     MOVE  SMS-F252          TO   SUB-F08.
*
*   サカタ２０分類
     MOVE  SMS-F262          TO   SUB-F09.
*
*   小売区分
     MOVE  SMS-F272          TO   SUB-F10.
*
*   振替区分
     MOVE  SMS-F282          TO   SUB-FIL1.
*
*   物流束区分
     MOVE  SMS-F292          TO   SUB-F89.
*
*   商品分類区分
     MOVE  SMS-F302          TO   SUB-F90.
*
*   管理区分
     MOVE  SMS-F312          TO   SUB-F91.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  SMS-F322          TO   SUB-F95.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*   登録日
     MOVE  SMS-F01           TO   SUB-F98.
*
*   更新日
*    F99:INIT
*
*------------------------------------------------------------
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  SMS-F342          TO   SUB-D01.
*
*   Ｄ３６５商品ＣＤ
     MOVE  SMS-F332          TO   SUB-D02.
*
*2020.05.28↓
*   オーダー区分
     MOVE  SMS-F352          TO   SUB-D03.
*2020.05.28↑
*
* -----------------------------------------------------------
*
*   登録担当者部門
     MOVE  SMS-F03           TO   SUB-D92.
*
*   登録担当者ＣＤ
     MOVE  SMS-F04           TO   SUB-D93.
*
*   登録日付
     MOVE  SMS-F01           TO   SUB-D94.
*
*   登録時刻
     MOVE  SMS-F02           TO   SUB-D95.
*
*   更新担当者部門
*    D96:INIT
*
*   更新担当者ＣＤ
*    D97:INIT
*
*   更新日付
*    D98:INIT
*
*   更新時刻
*    D99:INIT
*
*------------------------------------------------------------
*
*   サブ商品名称マスタ　ＷＲＩＴＥ
     WRITE      SUB-REC.
     ADD        1            TO   SUB-WT-CNT.
*
 SUB-NEW-EXIT.
     EXIT.
****************************************************************
*B  サブ商品名称マスタ　作成（更新モードでレコード非存在時）
*   　　　　　　　　　　　　　商品名称マスタ　から作成
****************************************************************
 SUB-NEW2-SEC        SECTION.
*
     MOVE "SUB-NEW2-SEC"     TO   S-NAME.
     INITIALIZE                   SUB-REC.
*
*   商品コード
     MOVE  MEI-F011          TO   SUB-F011.
*
*   品単コード
     MOVE  MEI-F012          TO   SUB-F012.
*
*   商品漢字名１
     MOVE  MEI-F021          TO   SUB-F021.
     IF    SMS-F151  =  "Y"
           MOVE  SMS-F152    TO   SUB-F021
     END-IF.
*
*   商品名漢字２
     MOVE  MEI-F022          TO   SUB-F022.
     IF    SMS-F161  =  "Y"
           MOVE  SMS-F162    TO   SUB-F022
     END-IF.
*
*   商品カナ名１
     MOVE  MEI-F031          TO   SUB-F031.
     IF    SMS-F171  =  "Y"
           MOVE  SMS-F172    TO   SUB-F031
     END-IF.
*
*   商品カナ名２
     MOVE  MEI-F032          TO   SUB-F032.
     IF    SMS-F181  =  "Y"
           MOVE  SMS-F182    TO   SUB-F032
     END-IF.
*
*   仕入単価
     MOVE  MEI-F041          TO   SUB-F041.
     IF    SMS-F191  =  "Y"
           MOVE  SMS-F192    TO   SUB-F041
     END-IF.
*
*   基準原価
     MOVE  MEI-F042          TO   SUB-F042.
     IF    SMS-F201  =  "Y"
           MOVE  SMS-F202    TO   SUB-F042
     END-IF.
*
*   基準売価
     MOVE  MEI-F043          TO   SUB-F043.
     IF    SMS-F211  =  "Y"
           MOVE  SMS-F212    TO   SUB-F043
     END-IF.
*
*   主仕入先コード
     MOVE  MEI-F05           TO   SUB-F05.
     IF    SMS-F221  =  "Y"
           MOVE  SMS-F222    TO   SUB-F05
     END-IF.
*
*   ＪＡＮコード
     MOVE  MEI-F06           TO   SUB-F06.
     IF    SMS-F231  =  "Y"
           MOVE  SMS-F232    TO   SUB-F06
     END-IF.
*
*   入数
     MOVE  MEI-F07           TO   SUB-F07.
     IF    SMS-F241  =  "Y"
           MOVE  SMS-F242    TO   SUB-F07
     END-IF.
* ----------------------------------------------------------
*
*   廃盤区分
     MOVE  MEI-F08           TO   SUB-F08.
     IF    SMS-F251  =  "Y"
           MOVE  SMS-F252    TO   SUB-F08
     END-IF.
*
*   サカタ２０分類
     MOVE  MEI-F09           TO   SUB-F09.
     IF    SMS-F261  =  "Y"
           MOVE  SMS-F262    TO   SUB-F09
     END-IF.
*
*   小売区分
     MOVE  MEI-F10           TO   SUB-F10.
     IF    SMS-F271  =  "Y"
           MOVE  SMS-F272    TO   SUB-F10
     END-IF.
*
*   振替区分
     MOVE  MEI-FIL1          TO   SUB-FIL1.
     IF    SMS-F281  =  "Y"
           MOVE  SMS-F282    TO   SUB-FIL1
     END-IF.
*
*   物流束区分
     MOVE  MEI-F89           TO   SUB-F89.
     IF    SMS-F291  =  "Y"
           MOVE  SMS-F292    TO   SUB-F89
     END-IF.
*
*   商品分類区分
     MOVE  MEI-F90           TO   SUB-F90.
     IF    SMS-F301  =  "Y"
           MOVE  SMS-F302    TO   SUB-F90
     END-IF.
*
*   管理区分
     MOVE  MEI-F91           TO   SUB-F91.
     IF    SMS-F311  =  "Y"
           MOVE  SMS-F312    TO   SUB-F91
     END-IF.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  MEI-F95           TO   SUB-F95.
     IF    SMS-F321  =  "Y"
           MOVE  SMS-F322    TO   SUB-F95
     END-IF.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*   登録日
     MOVE  SMS-F01           TO   SUB-F98.
*
*   更新日
     MOVE  SMS-F01           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  SPACE             TO   SUB-D01.
     IF    SMS-F341  =  "Y"
           MOVE  SMS-F342    TO   SUB-D01
     END-IF.
*
*   Ｄ３６５商品ＣＤ
     MOVE  SPACE             TO   SUB-D02.
     IF    SMS-F331  =  "Y"
           MOVE  SMS-F332    TO   SUB-D02
     END-IF.
*
*2020.05.28↓
*   オーダー区分
     MOVE  SPACE             TO   SUB-D03.
     IF    SMS-F351  =  "Y"
           MOVE  SMS-F352    TO   SUB-D03
     END-IF.
*2020.05.28↑
*
*------------------------------------------------------------
*
*   登録担当者部門
     MOVE  SMS-F03           TO   SUB-D92.
*
*   登録担当者ＣＤ
     MOVE  SMS-F04           TO   SUB-D93.
*
*   登録日付
     MOVE  SMS-F01           TO   SUB-D94.
*
*   登録時刻
     MOVE  SMS-F02           TO   SUB-D95.
*
*   更新担当者部門
     MOVE  SMS-F03           TO   SUB-D96.
*
*   更新担当者ＣＤ
     MOVE  SMS-F04           TO   SUB-D97.
*
*   更新日付
     MOVE  SMS-F01           TO   SUB-D98.
*
*   更新時刻
     MOVE  SMS-F02           TO   SUB-D99.
*
*------------------------------------------------------------
*
*   サブ商品名称マスタ　ＷＲＩＴＥ
     WRITE      SUB-REC.
     ADD        1            TO   SUB-WT-CNT.
*
 SUB-NEW2-EXIT.
     EXIT.
****************************************************************
*C  商品名称マスタ　更新（更新モードでレコード存在時）
*   　　　　　　　　　　　取込ファイルより更新
****************************************************************
 MEI-UPD-SEC        SECTION.
*
     MOVE "MEI-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   MEI-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   MEI-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   MEI-F012.
*
*   商品名漢字１
     IF    SMS-F151  =  "Y"
           MOVE  SMS-F152    TO   MEI-F021
     END-IF.
*
*   商品名漢字２
     IF    SMS-F161  =  "Y"
           MOVE  SMS-F162    TO   MEI-F022
     END-IF.
*
*   商品カナ名１
     IF    SMS-F171  =  "Y"
           MOVE  SMS-F172    TO   MEI-F031
     END-IF.
*   商品カナ名２
     IF    SMS-F181  =  "Y"
           MOVE  SMS-F182    TO   MEI-F032
     END-IF.
*
*   仕入単価
     IF    SMS-F191  =  "Y"
           MOVE  SMS-F192    TO   MEI-F041
     END-IF.
*
*   基準原価
     IF    SMS-F201  =  "Y"
           MOVE  SMS-F202    TO   MEI-F042
     END-IF.
*
*   基準売価
     IF    SMS-F211  =  "Y"
           MOVE  SMS-F212    TO   MEI-F043
     END-IF.
*
*   主仕入先コード
     IF    SMS-F221  =  "Y"
           MOVE  SMS-F222    TO   MEI-F05
     END-IF.
*
*   ＪＡＮコード
     IF    SMS-F231  =  "Y"
           MOVE  SMS-F232    TO   MEI-F06
     END-IF.
*
*   入数
     IF    SMS-F241  =  "Y"
           MOVE  SMS-F242    TO   MEI-F07
     END-IF.
* -----------------------------------------
*   廃盤区分
     IF    SMS-F251  =  "Y"
           MOVE  SMS-F252    TO   MEI-F08
     END-IF.
*
*   サカタ２０分類
     IF    SMS-F261  =  "Y"
           MOVE  SMS-F262    TO   MEI-F09
     END-IF.
*
*   小売区分
     IF    SMS-F271  =  "Y"
           MOVE  SMS-F272    TO   MEI-F10
     END-IF.
*
*   振替区分
     IF    SMS-F281  =  "Y"
           MOVE  SMS-F282    TO   MEI-FIL1
     END-IF.
*
*   物流束区分
     IF    SMS-F291  =  "Y"
           MOVE  SMS-F292    TO   MEI-F89
     END-IF.
*
*   商品分類区分
     IF    SMS-F301  =  "Y"
           MOVE  SMS-F302    TO   MEI-F90
     END-IF.
*
*   管理区分
     IF    SMS-F311  =  "Y"
           MOVE  SMS-F312    TO   MEI-F91
     END-IF.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     IF    SMS-F321  =  "Y"
           MOVE  SMS-F322    TO   MEI-F95
     END-IF.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
* -----------------------------------------------
*
*   登録日
*    F98:-
*
*   更新日
     MOVE  SMS-F01           TO   MEI-F99.
*
*------------------------------------------------------------
*
*   商品名称マスタ　ＲＥＷＲＩＴＥ
     REWRITE    MEI-REC.
     ADD        1            TO   MEI-RT-CNT.
*
 MEI-UPD-EXIT.
     EXIT.
*
****************************************************************
*D  商品名称マスタ　更新（登録モードでレコード存在時）
*   　　　　　　　　　　　取込ファイルより全項目置き換え
****************************************************************
 MEI-UPD2-SEC        SECTION.
*
     MOVE "MEI-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   MEI-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   MEI-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   MEI-F012.
*
*   商品名漢字１
     MOVE  SMS-F152          TO   MEI-F021.
*
*   商品名漢字２
     MOVE  SMS-F162          TO   MEI-F022.
*
*   商品カナ名１
     MOVE  SMS-F172          TO   MEI-F031.
*
*   商品カナ名２
     MOVE  SMS-F182          TO   MEI-F032.
*
*   仕入単価
     MOVE  SMS-F192          TO   MEI-F041.
*
*   基準原価
     MOVE  SMS-F202          TO   MEI-F042.
*
*   基準売価
     MOVE  SMS-F212          TO   MEI-F043.
*
*   主仕入先コード
     MOVE  SMS-F222          TO   MEI-F05.
*
*   ＪＡＮコード
     MOVE  SMS-F232          TO   MEI-F06.
*
*   入数
     MOVE  SMS-F242          TO   MEI-F07.
* -----------------------------------------
*   廃盤区分
     MOVE  SMS-F252          TO   MEI-F08.
*
*   サカタ２０分類
     MOVE  SMS-F262          TO   MEI-F09.
*
*   小売区分
     MOVE  SMS-F272          TO   MEI-F10.
*
*   振替区分
     MOVE  SMS-F282          TO   MEI-FIL1.
*
*   物流束区分
     MOVE  SMS-F292          TO   MEI-F89.
*
*   商品分類区分
     MOVE  SMS-F302          TO   MEI-F90.
*
*   管理区分
     MOVE  SMS-F312          TO   MEI-F91.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  SMS-F322          TO   MEI-F95.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*------------------------------------------------------------
*
*   登録日
*    F98:-
*
*   更新日
     MOVE  SMS-F01           TO   MEI-F99.
*
*------------------------------------------------------------
*
*   商品名称マスタ　ＲＥＷＲＩＴＥ
     REWRITE    MEI-REC.
     ADD        1            TO   MEI-RT-CNT.
*
 MEI-UPD2-EXIT.
     EXIT.
*
****************************************************************
*C  サブ商品名称マスタ　更新（更新モードでレコード存在時）
*   　　　　　　　　　　　　　取込ファイルより更新
****************************************************************
 SUB-UPD-SEC        SECTION.
*
     MOVE "SUB-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SUB-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   SUB-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   SUB-F012.
*
*   商品名漢字１
     IF    SMS-F151  =  "Y"
           MOVE  SMS-F152    TO   SUB-F021
     END-IF.
*
*   商品名漢字２
     IF    SMS-F161  =  "Y"
           MOVE  SMS-F162    TO   SUB-F022
     END-IF.
*
*   商品カナ名１
     IF    SMS-F171  =  "Y"
           MOVE  SMS-F172    TO   SUB-F031
     END-IF.
*
*   商品カナ名２
     IF    SMS-F181  =  "Y"
           MOVE  SMS-F182    TO   SUB-F032
     END-IF.
*
*   仕入単価
     IF    SMS-F191  =  "Y"
           MOVE  SMS-F192    TO   SUB-F041
     END-IF.
*
*   基準原価
     IF    SMS-F201  =  "Y"
           MOVE  SMS-F202    TO   SUB-F042
     END-IF.
*
*   基準売価
     IF    SMS-F211  =  "Y"
           MOVE  SMS-F212    TO   SUB-F043
     END-IF.
*
*   主仕入先コード
     IF    SMS-F221  =  "Y"
           MOVE  SMS-F222    TO   SUB-F05
     END-IF.
*
*   ＪＡＮコード
     IF    SMS-F231  =  "Y"
           MOVE  SMS-F232    TO   SUB-F06
     END-IF.
*
*   入数
     IF    SMS-F241  =  "Y"
           MOVE  SMS-F242    TO   SUB-F07
     END-IF.
*
* ----------------------------------------------------------
*
*   廃盤区分
     IF    SMS-F251  =  "Y"
           MOVE  SMS-F252    TO   SUB-F08
     END-IF.
*
*   サカタ２０分類
     IF    SMS-F261  =  "Y"
           MOVE  SMS-F262    TO   SUB-F09
     END-IF.
*
*   小売区分
     IF    SMS-F271  =  "Y"
           MOVE  SMS-F272    TO   SUB-F10
     END-IF.
*
*   振替区分
     IF    SMS-F281  =  "Y"
           MOVE  SMS-F282    TO   SUB-FIL1
     END-IF.
*
*   物流束区分
     IF    SMS-F291  =  "Y"
           MOVE  SMS-F292    TO   SUB-F89
     END-IF.
*
*   商品分類区分
     IF    SMS-F301  =  "Y"
           MOVE  SMS-F302    TO   SUB-F90
     END-IF.
*
*   管理区分
     IF    SMS-F311  =  "Y"
           MOVE  SMS-F312    TO   SUB-F91
     END-IF.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     IF    SMS-F321  =  "Y"
           MOVE  SMS-F322    TO   SUB-F95
     END-IF.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*   登録日
*    F98:非更新
*
*   更新日
     MOVE  SMS-F01           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   Ｄ３６５ＪＡＮＣＤ
     IF    SMS-F341  =  "Y"
           MOVE  SMS-F342    TO   SUB-D01
     END-IF.
*
*   Ｄ３６５商品ＣＤ
     IF    SMS-F331  =  "Y"
           MOVE  SMS-F332    TO   SUB-D02
     END-IF.
*
*2020.05.28↓
*   オーダー区分
     IF    SMS-F351  =  "Y"
           MOVE  SMS-F352    TO   SUB-D03
     END-IF.
*2020.05.28↑
*
*-----------------------------------------------------------
*
*   登録担当者部門
*    D92:非更新
*
*   登録担当者ＣＤ
*    D93:非更新
*
*   登録日付
*    D94:非更新
*
*   登録時刻
*    D95:非更新
*
*   更新担当者部門
     MOVE  SMS-F03           TO   SUB-D96.
*
*   更新担当者ＣＤ
     MOVE  SMS-F04           TO   SUB-D97.
*
*   更新日付
     MOVE  SMS-F01           TO   SUB-D98.
*
*   更新時刻
     MOVE  SMS-F02           TO   SUB-D99.
*
*------------------------------------------------------------
*
*   サブ商品名称マスタ　ＲＥＷＲＩＴＥ
     REWRITE    SUB-REC.
     ADD        1            TO   SUB-RT-CNT.
*
 SUB-UPD-EXIT.
     EXIT.
*
****************************************************************
*D  サブ商品名称マスタ　更新（登録モードでレコード存在時）
*   　　　　　　　　　　　　　取込ファイルより全項目置き換え
****************************************************************
 SUB-UPD2-SEC        SECTION.
*
     MOVE "SUB-UPD-SEC"      TO   S-NAME.
*    INITIALIZE                   SUB-REC.
*
*   商品コード
     MOVE  SMS-F13           TO   SUB-F011.
*
*   品単コード
     MOVE  SMS-F14           TO   SUB-F012.
*
*   商品名漢字１
     MOVE  SMS-F152          TO   SUB-F021.
*
*   商品名漢字２
     MOVE  SMS-F162          TO   SUB-F022.
*
*   商品カナ名１
     MOVE  SMS-F172          TO   SUB-F031.
*
*   商品カナ名２
     MOVE  SMS-F182          TO   SUB-F032.
*
*   仕入単価
     MOVE  SMS-F192          TO   SUB-F041.
*
*   基準原価
     MOVE  SMS-F202          TO   SUB-F042.
*
*   基準売価
     MOVE  SMS-F212          TO   SUB-F043.
*
*   主仕入先コード
     MOVE  SMS-F222          TO   SUB-F05.
*
*   ＪＡＮコード
     MOVE  SMS-F232          TO   SUB-F06.
*
*   入数
     MOVE  SMS-F242          TO   SUB-F07.
*
* -----------------------------------------------------------
*
*   廃盤区分
     MOVE  SMS-F252          TO   SUB-F08.
*
*   サカタ２０分類
     MOVE  SMS-F262          TO   SUB-F09.
*
*   小売区分
     MOVE  SMS-F272          TO   SUB-F10.
*
*   振替区分
     MOVE  SMS-F282          TO   SUB-FIL1.
*
*   物流束区分
     MOVE  SMS-F292          TO   SUB-F89.
*
*   商品分類区分
     MOVE  SMS-F302          TO   SUB-F90.
*
*   管理区分
     MOVE  SMS-F312          TO   SUB-F91.
*
*   自動発注区分
*    F92:INIT
*
*   商品名称区分
*    F93:INIT
*
*   ユポラベル区分
*    F94:INIT
*
*   定番区分
     MOVE  SMS-F322          TO   SUB-F95.
*
*   季節区分
*    F96:INIT
*
*   予備区分３
*    F97:INIT
*
*   登録日
*    F98:非更新
*
*   更新日
     MOVE  SMS-F01           TO   SUB-F99.
*
*------------------------------------------------------------
*
*   Ｄ３６５ＪＡＮＣＤ
     MOVE  SMS-F342          TO   SUB-D01.
*
*   Ｄ３６５商品ＣＤ
     MOVE  SMS-F332          TO   SUB-D02.
*
*2020.05.28↓
*   オーダー区分
     MOVE  SMS-F352          TO   SUB-D03.
*2020.05.28↑
*------------------------------------------------------------
*
*   登録担当者部門
*    D92:非更新
*
*   登録担当者ＣＤ
*    D93:非更新
*
*   登録日付
*    D94:非更新
*
*   登録時刻
*    D95:非更新
* -----------------------------------------------------------
*   更新担当者部門
     MOVE  SMS-F03           TO   SUB-D96.
*
*   更新担当者ＣＤ
     MOVE  SMS-F04           TO   SUB-D97.
*
*   更新日付
     MOVE  SMS-F01           TO   SUB-D98.
*
*   更新時刻
     MOVE  SMS-F02           TO   SUB-D99.
*
*------------------------------------------------------------
*
*   サブ商品名称マスタ　ＲＥＷＲＩＴＥ
     REWRITE    SUB-REC.
     ADD        1            TO   SUB-RT-CNT.
*
 SUB-UPD2-EXIT.
     EXIT.
*
****************************************************************
*    商品名称マスタ検索
****************************************************************
 MEI-READ-SEC               SECTION.
     MOVE     "MEI-READ-SEC" TO   S-NAME.
*
     READ     MEIMS1
       INVALID
              MOVE "INV"     TO   MEI-FLG
       NOT  INVALID
              MOVE SPACE     TO   MEI-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ検索
****************************************************************
 SUB-READ-SEC               SECTION.
     MOVE     "SUB-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL1
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
     MOVE      SMS-RD-CNT   TO    MSG-OUT01.
     MOVE      MEI-WT-CNT   TO    MSG-OUT02.
     MOVE      MEI-RT-CNT   TO    MSG-OUT03.
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
     CLOSE     SMSMNTW1
               MEIMS1
               SUBMEIL1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
