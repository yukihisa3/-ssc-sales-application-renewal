# SPD0140I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD0140I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　取込確認リスト　　　　　　　　　　*
*    作成日／作成者　　　：　09/10/30  OONO                    *
*    　　　　　　　　　　：　画面日付，時刻表示　　　　　　　　*
*                        ：　日付変換サブルーチン使用　　　　　*
*    再利用ＰＧ　　　　　：  SSKT040.SKTSLIB                   *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　画面よりＰＣ側手書取込データ変換を*
*                        ：　行う取引先ＣＤ、メモ_の範囲を入力*
*            　　　　　　：　して、量販店ＪＮＬ（ＰＣ手書連携用*
*                        ：　）で存在チェックを行う。存在した場*
*                        ：　合のみ、パラメタに取引先ＣＤ、メモ*
*            　　　　　　：　_の範囲を渡す。ＰＦ５：終了で終わ*
*                        ：　りにした場合は、PROGRAM-STATUS    *
*                        ：　に４０１０を送り、ＰＧを終了する。*
*    更新日／更新者　　　：　10/02/16 OONO                     *
*    処理概要　　　　　　：　画面より分類コード・伝票区分を受け*
*                        ：　取り、パラメタに渡す　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD0140I.
 AUTHOR.                OONO.
 DATE-WRITTEN.          09/10/30.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         21-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 量販店ＪＮＬ（ＰＣ手書連携用） >>--*
     SELECT   PCRYOJF   ASSIGN         DA-01-VI-PCRYOJL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  PCR-F11  PCR-F011
                                       PCR-F012 PCR-F02
                        STATUS         PCR-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FPD01401  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 量販店ＪＮＬ（ＰＣ手書連携用） >>--*
 FD  PCRYOJF            LABEL     RECORD   IS   STANDARD.
     COPY     PCRYOJF   OF        XFDLIB
              JOINING   PCR       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLGS.
     03  END-FLG        PIC  X(03)    VALUE  SPACE.
     03  ERR-FLG        PIC  9(01)    VALUE  ZERO.
*
*----<< ファイルステータス >>------
 01  TOK-ST            PIC  X(02).
 01  PCR-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE.
     03  WK-YS          PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y       PIC  9(02)  VALUE ZERO.
         05  WK-M       PIC  9(02)  VALUE ZERO.
         05  WK-D       PIC  9(02)  VALUE ZERO.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*----<< ﾊﾝｲ ﾜｰｸ >>--*
 01  MEMO-X.
     03  ST-MEMO        PIC  ZZZ9.
     03  EN-MEMO        PIC  ZZZ9.
 01  MEMO-Y         REDEFINES  MEMO-X.
     03  ST-MEMO2       PIC  9(04).
     03  EN-MEMO2       PIC  9(04).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
     03  PF09           PIC  X(04)     VALUE     "F009".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(30)  VALUE   NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(30)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                            SECTION.
 01  PARA-TORICD           PIC 9(08).
 01  PARA-MEMOST           PIC 9(04).
 01  PARA-MEMOEN           PIC 9(04).
 01  PARA-BUNCD            PIC X(04).
 01  PARA-DENKU            PIC X(02).
****************************************************************
 PROCEDURE         DIVISION  USING     PARA-TORICD  PARA-MEMOST
                                                    PARA-MEMOEN
                                       PARA-BUNCD   PARA-DENKU .
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SPD0140I DSPFILE ERROR " DSP-CNTL " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    DSPFILE HTOKMS PCRYOJF.
*
     STOP     RUN.
*----<< 表示ファイル >>--*
 HTOKMS-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SPD0140I HTOKMS  ERROR " DSP-CNTL " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    DSPFILE HTOKMS PCRYOJF.
*
     STOP     RUN.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCRYOJF.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SPD0140I PCRYOJF ERROR " DSP-CNTL " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    DSPFILE HTOKMS PCRYOJF.
*
     STOP     RUN.
*
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      初期処理                                    *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時刻取得
     ACCEPT    SYS-TIME         FROM   TIME.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*
     DISPLAY  "*** SPD0140I START *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" WK-M ":" SYS-SS UPON CONS.
*
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS    PCRYOJF.
*
     INITIALIZE        FLGS.
*
*----<< 制御区分 >>-*
     MOVE     1              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*----<< 画面初期化処理 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    3.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      終了処理                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSPFILE HTOKMS PCRYOJF.
*
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SPD0140I END *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期処理　　　　                        *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FPD01401.
*システム日付転送
     MOVE     HEN-DATE       TO   SDATE.
*システム時間転送
     MOVE    HEN-TIME        TO   STIME.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FPD01401"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
*エラー初期化
     MOVE     ZERO           TO   ERR-FLG.
*項目制御クリア
     PERFORM  DSP-SYOKI-SEC.
*
     MOVE     2              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      範囲指定入力処理                            *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      1         TO   GR-NO
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE     "END"      TO   END-FLG
              MOVE      0         TO   GR-NO
         WHEN ENT
              PERFORM   220-HANNI-CHECK
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      範囲指定項目チェック処理                    *
*--------------------------------------------------------------*
 220-HANNI-CHECK         SECTION.
*項目制御・エラーフラグのクリア
     PERFORM  DSP-SYOKI-SEC.
     MOVE     ZERO           TO   ERR-FLG.
*
*  取引先ＣＤの入力チェック
     IF        TORICD    IS NOT  NUMERIC
               MOVE      ZERO      TO   TORICD
     END-IF.
*  取引先マスタの存在チェック
     MOVE      TORICD    TO   TOK-F01
     READ      HTOKMS
           INVALID  KEY
               MOVE      1    TO   ERR-FLG
               MOVE    NC"取引先マスタ未登録です。"
                                  TO   MSG
               MOVE     "R"   TO   EDIT-OPTION OF TORICD
*              取引先名を初期化します
               MOVE      SPACE     TO   TORINM
            NOT INVALID  KEY
               MOVE      TOK-F02   TO   TORINM
     END-READ.
     IF        SMEMO     IS NOT   NUMERIC
               MOVE      ZERO      TO   SMEMO
     END-IF.
     IF        EMEMO     IS NOT  NUMERIC
               MOVE      9999      TO   EMEMO
     END-IF.
     IF        SMEMO    >   EMEMO
             MOVE      1    TO   ERR-FLG
             MOVE    NC"メモ_開始が終了を超えています。"
                                           TO   MSG
             MOVE     "R"   TO   EDIT-OPTION OF SMEMO
             MOVE     "R"   TO   EDIT-OPTION OF EMEMO
     END-IF.
*  エラーのない時は量販店ＪＮＬをメモ_の範囲で索引
     IF        ERR-FLG   =  ZERO
         MOVE      TORICD    TO   PCR-F11
         MOVE      SMEMO     TO   PCR-F011
         MOVE      ZERO      TO   PCR-F012  PCR-F02
*
         START    PCRYOJF   KEY  >=   PCR-F11   PCR-F011
                                      PCR-F012  PCR-F02
               INVALID
                   MOVE  NC"対象データ無し"  TO   MSG
                   GO         TO   220-HANNI-CHECK-EXIT
         END-START
*      量販店ＪＮＬの読み込み
         READ     PCRYOJF    NEXT AT  END
                  MOVE  NC"対象データ無し"  TO   MSG
                  GO         TO   220-HANNI-CHECK-EXIT
         END-READ
*      読込んだ量販店ＪＮＬ取引先ＣＤがパラメタと同じかチェック
         IF       PCR-F11  >   TORICD
                  MOVE  NC"対象データ無し"  TO   MSG
                  GO         TO   220-HANNI-CHECK-EXIT
         END-IF
*      読込んだ量販店ＪＮＬがメモ_範囲内かチェック
         IF       SMEMO    <=  PCR-F011
         AND      PCR-F011 <=  EMEMO
                  MOVE       3        TO   GR-NO
                  GO         TO   220-HANNI-CHECK-EXIT
         ELSE
                  MOVE  NC"対象データ無し"  TO   MSG
                  GO         TO   220-HANNI-CHECK-EXIT
         END-IF
*
      END-IF.
 220-HANNI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認処理　                                  *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      1         TO   GR-NO
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE     "END"      TO   END-FLG
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      2         TO   GR-NO
         WHEN ENT
              MOVE      TORICD    TO   PARA-TORICD
              MOVE      SMEMO     TO   PARA-MEMOST
              MOVE      EMEMO     TO   PARA-MEMOEN
*             分類コード・伝票区分のﾊﾟﾗﾒﾀ初期化
              MOVE      SPACE     TO   PARA-BUNCD PARA-DENKU
*        ADD NAV OONO 2010/02/16 BGN
*             分類コード・伝票区分が空白でない時パラメタに渡す
              IF BUNCD NOT = SPACE
                 MOVE      BUNCD     TO   PARA-BUNCD
              END-IF
              IF DENKU NOT = SPACE
                 MOVE      DENKU     TO   PARA-DENKU
              END-IF
*        ADD NAV OONO 2010/02/16 END
*
              MOVE     "END"      TO   END-FLG
              MOVE      0         TO   GR-NO
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "SCRERE"       TO   DSP-GRP.
     IF       GR-NO     =    2
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     PERFORM  900-DSP-WRITE.
**
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.

     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FPD01401.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面制御項目初期化                          *
*--------------------------------------------------------------*
 DSP-SYOKI-SEC          SECTION.
*  取引先ＣＤ項目
     MOVE     "M"       TO   EDIT-OPTION OF TORICD
     MOVE      SPACE    TO   EDIT-CURSOR OF TORICD
*  メモ_開始項目
     MOVE     "M"       TO   EDIT-OPTION OF SMEMO
     MOVE      SPACE    TO   EDIT-CURSOR OF SMEMO
*  メモ_終了項目
     MOVE     "M"       TO   EDIT-OPTION OF EMEMO
     MOVE      SPACE    TO   EDIT-CURSOR OF EMEMO
 DSP-SYOKI-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
