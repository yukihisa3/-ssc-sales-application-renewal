***************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＩＴ統制システム　                *
*    業務名　　　　　　　：　条件ファイル　　                  *
*    モジュール名　　　　：　条件ファイルリスト（連携）　　    *
*    作成日／作成者　　　：　09/03/25-04/01  NAV               *
*    更新日／更新者　　　：                                    *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
***************************************************************
******************************************************************
*                                                                *
*             IDENTIFICATION      DIVISION                       *
*                                                                *
******************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SIT0120L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/03/25.
******************************************************************
*                                                                *
*             ENVIRONMENT         DIVISION                       *
*                                                                *
******************************************************************
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K.
 OBJECT-COMPUTER.       FACOM-K.
 SPECIAL-NAMES.
         YA      IS     PITCH-2
         YB      IS     PITCH-15
         YB-21   IS     BAIKAKU
         CONSOLE IS     CONS
         STATION IS     STA.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  マスタ更新履歴ファイル　>>**************************
     SELECT   MSTLOGF   ASSIGN  TO   DA-01-VI-MSTLOGL3
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  LOG-F01
                                                 LOG-F02
                                                 LOG-F03
                                                 LOG-F05
                                                 LOG-F06
                                                 LOG-F07
                        FILE STATUS          IS   LOG-ST.
****<< 担当者マスタファイル >>******************************
     SELECT   HTANMS    ASSIGN  TO   DA-01-VI-TANMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TAN-F01
                                                  TAN-F02
                        FILE    STATUS       IS   TAN-ST.
*---------   プリンター　ファイル
     SELECT   PRTFILE   ASSIGN    TO        LP-04.
******************************************************************
*                                                                *
*             DATA                DIVISION                       *
*                                                                *
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
****<<  マスタ更新履歴ファイル　>>**************************
 FD    MSTLOGF
       LABEL     RECORD     IS   STANDARD.
       COPY      MSTLOGF    OF  XFDLIB
       JOINING   LOG        AS  PREFIX.
****<< 担当者マスタファイル >>******************************
 FD    HTANMS
       BLOCK     CONTAINS   2   RECORDS.
       COPY      HTANMS     OF  XFDLIB
       JOINING   TAN        AS  PREFIX.
*------   ﾌﾟﾘﾝﾀ   ﾌｧｲﾙ
 FD  PRTFILE
     LABEL    RECORD    IS        OMITTED
     LINAGE   IS   66.
 01  PRTF-REC.
    03  FILLER          PIC   X(200).
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*01  DSP-AREA.
*    03  DSP-FMT             PIC  X(08)  VALUE   SPACE.
*    03  DSP-GRP             PIC  X(08)  VALUE   SPACE.
*    03  DSP-WS              PIC  X(08)  VALUE   SPACE.
*    03  DSP-WSR  REDEFINES  DSP-WS.
*        05  DSP-WS1         PIC  X(02).
*        05  DSP-WS2         PIC  9(03).
*        05  DSP-WS3         PIC  X(01).
*    03  DSP-PRO             PIC  X(02)  VALUE   SPACE.
*    03  DSP-UNIT            PIC  X(06)  VALUE   SPACE.
*    03  DSP-FNC             PIC  X(04)  VALUE   SPACE.
*    03  DSP-ST              PIC  X(02)  VALUE   SPACE.
*    03  DSP-ST1             PIC  X(04)  VALUE   SPACE.
 01  WOAK-ERIA.
     03  CNT-PAGE            PIC   9(03).
     03  MSG-FLG             PIC   9(01).
     03  MI-FLG              PIC   9(01).
     03  END-FLG             PIC   9(01).
     03  IN-DATA             PIC   X(01).
     03  LOG-ST              PIC   X(02).
     03  TAN-ST              PIC   X(02).
     03  JYO-ST2             PIC   X(04).
     03  WK-KEY1.
         05  WK-R1           PIC   9(02).
         05  WK-R2           PIC   X(06).
     03  WK-KEY2.
         05  WK-R4           PIC   9(02).
         05  WK-R5           PIC   X(06).
     03  FILE-ERR001         PIC   N(10)    VALUE
                         NC"更新履歴Ｆ異常！".
     03  FILE-ERR002         PIC   N(10)    VALUE
                         NC"担当社マスタ異常！".
***  条件ファイル用履歴エリア
     COPY  HJYOKEN     OF   XFDLIB
     JOINING   WKJYO    AS  PREFIX.
*
*01  SISTEM-HIZUKE.
*    03  SYS-DATE            PIC   9(06).
*    03  SYS-DATE-R  REDEFINES  SYS-DATE.
*        05  SYS-YY          PIC   9(02).
*        05  SYS-MM          PIC   9(02).
*        05  SYS-DD          PIC   9(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
**
 01  READ-CNT                     PIC  9(07)  VALUE  ZERO.
 01  IN-CNT                       PIC  9(07)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
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
*------  ﾌﾟﾘﾝﾀ  出力用
 01  MEISAI-1.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-3           PIC   9(02).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-4           PIC   X(08).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  FILLER   CHARACTER  TYPE  PITCH-15.
         05  JO-5       PIC   N(20).
         05  DMY        PIC   N(01).
     03  FILLER         PIC   X(09)   VALUE SPACE.
     03  JO-6           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-7           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-8           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-9           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-10          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-11          PIC   ZZZZZZZZZ.ZZ-.
 01  MEISAI-2.
     03  FILLER         PIC   X(13)   VALUE SPACE.
     03  FILLER   CHARACTER  TYPE  PITCH-15.
         05  JO-15      PIC   N(05).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-16          PIC   X(15).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-17          PIC   X(15).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-12          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-13          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-14          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-18          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-19          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-20          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(07)   VALUE SPACE.
****  明細行２１             ****
 01  MEISAI-21      CHARACTER     TYPE   IS   PITCH-15.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-201             PIC  N(04).
     02  MEI-202             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-203             PIC  N(11).
     02  MEI-204             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-205             PIC  X(08).
     02  FILLER              PIC  N(01)  VALUE  NC"］".
 01  OMIDASHI.
    03  FILLER       PIC   X(27)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   BAIKAKU.
       05  FILLER    PIC   N(07)    VALUE
                     NC"＊＊　条　件　".
       05  FILLER    PIC   N(16)    VALUE
                     NC"フ　ァ　イ　ル　リ　ス　ト　＊＊".
    03  FILLER       PIC   X(14)    VALUE  SPACE.
    03  OMI-YY       PIC   Z9.
    03  FILLER       PIC   X(01)    VALUE  "/".
    03  OMI-MM       PIC   Z9.
    03  FILLER       PIC   X(01)    VALUE  "/".
    03  OMI-DD       PIC   Z9.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(02)    VALUE  NC"作成".
       05  FILLER    PIC   X(02)    VALUE  SPACE.
       05  OMI-PAGE  PIC   ZZZ9.
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(01)    VALUE  NC"頁".
****  見出し行１２　１４　１９***
 01  MIDASI-12.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行１３           ****
 01  MIDASI-13.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  PITCH-2    VALUE
         NC"責任者".
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  PITCH-2    VALUE
         NC"担当者".
     02  FILLER              PIC  X(01)  VALUE  "!".
***  見出し行１５ １６ １７ １８
 01  MIDASI-15.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
*
 01  KOMIDASHI-1.
    03  FILLER       PIC   X(01)    VALUE  SPACE.
    03  FILLER       PIC   X(04)    VALUE  "KEY1".
    03  FILLER       PIC   X(01)    VALUE  SPACE.
    03  FILLER       PIC   X(04)    VALUE  "KEY2".
    03  FILLER       PIC   X(03)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   N(04)    VALUE  NC"日本語１".
       05  FILLER    PIC   X(38)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字１".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字２".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字３".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字４".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字５".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字６".
 01  KOMIDASHI-2.
    03  FILLER       PIC   X(13)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   N(04)    VALUE  NC"日本語２".
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"英数字".
       05  FILLER    PIC   X(10)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"英数字".
       05  FILLER    PIC   X(15)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字７".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字８".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字９".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ａ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ｂ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ｃ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
****  見出し行２１           ****
 01  MIDASI-21      CHARACTER     TYPE   IS   PITCH-15.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER                   PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM              PIC  N(02).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM                PIC  N(10).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
 LINKAGE          SECTION.
 01  PARA-BUMONCD          PIC X(04).
*01  PARA-TANCD            PIC X(02).
 01  PARA-TANCD            PIC X(08).
*01  PARA-MKUBUN           PIC 9(02).
 01  PARA-UPDDATES         PIC 9(08).
 01  PARA-UPDTIMES         PIC 9(06).
 01  PARA-UPDDATEE         PIC 9(08).
 01  PARA-UPDTIMEE         PIC 9(06).
******************************************************************
*                                                                *
*             PROCEDURE           DIVISION                       *
*                                                                *
******************************************************************
*
 PROCEDURE              DIVISION  USING PARA-BUMONCD PARA-TANCD
                        PARA-UPDDATES  PARA-UPDTIMES
                        PARA-UPDDATEE  PARA-UPDTIMEE.
*------  エラー処理
 DECLARATIVES.
 000-TAN-ERR            SECTION.
     USE  AFTER    EXCEPTION      PROCEDURE    HTANMS.
     DISPLAY       TAN-ST         UPON  STA.
     DISPLAY       FILE-ERR001    UPON  STA.
     ACCEPT        IN-DATA        FROM  STA.
     STOP  RUN.
 000-DSP-ERR            SECTION.
     USE  AFTER    EXCEPTION      PROCEDURE    MSTLOGF.
     DISPLAY       LOG-ST         UPON  STA.
     DISPLAY       FILE-ERR002    UPON  STA.
     ACCEPT        IN-DATA        FROM  STA.
     STOP  RUN.
 END DECLARATIVES.
****************************************************************
*        処理                                                  *
****************************************************************
 SHORI-SEC              SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM  END-SEC.
     STOP     RUN.
 SHORI-EXIT.
     EXIT.
****************************************************************
*                      初期処理                                *
****************************************************************
 INIT-SEC               SECTION.
*------  ﾌｧｲﾙ  OPEN
     OPEN     INPUT     MSTLOGF.
     OPEN     OUTPUT    PRTFILE.
     OPEN     INPUT     HTANMS.
*------  変数  ｸﾘｱ
     MOVE     ZERO      TO   MI-FLG  CNT-PAGE  END-FLG  MSG-FLG.

*------  ｼｽﾃﾑ日付をとる
*****ACCEPT   SYS-DATE  FROM      DATE.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE     WK-Y      TO        OMI-YY.
     MOVE     WK-M      TO        OMI-MM.
     MOVE     WK-D      TO        OMI-DD.
*** マスタ更新履歴ＳＴＡＲＴ
     MOVE     "05"              TO    LOG-F01.
     MOVE     PARA-BUMONCD      TO    LOG-F02.
     MOVE     PARA-TANCD        TO    LOG-F03.
     MOVE     PARA-UPDDATES     TO    LOG-F05.
     MOVE     PARA-UPDTIMES     TO    LOG-F06.
     MOVE     ZERO              TO    LOG-F07.
*
     START    MSTLOGF   KEY  IS  >=  LOG-F01
                                     LOG-F02
                                     LOG-F03
                                     LOG-F05
                                     LOG-F06
                                     LOG-F07
              INVALID   MOVE    9    TO  END-FLG
                        GO   TO   INIT-EXIT
     END-START.
     PERFORM       READ-LOG-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC               SECTION.
**   MOVE   WKJYO-F01     TO   WK-R1.
**   MOVE   WKJYO-F02     TO   WK-R2.
     IF   MI-FLG    =   ZERO
        THEN
              PERFORM   MIDASI-SEC.
     IF   LINAGE-COUNTER  >  62
              MOVE      SPACE          TO   PRTF-REC
              WRITE     PRTF-REC    AFTER   PAGE
              PERFORM   MIDASI-SEC.
     PERFORM  MEISAI-SEC.
*
     PERFORM      READ-LOG-SEC.
*
 MAIN-EXIT.
      EXIT.
****************************************************************
*                       ＥＮＤ処理                             *
****************************************************************
 END-SEC           SECTION.
     CLOSE    MSTLOGF    PRTFILE     HTANMS.
**
     DISPLAY  "SIT0120L READ ="  READ-CNT  UPON  STA.
     DISPLAY  "SIT0120L IN   ="  IN-CNT    UPON  STA.
 END-EXIT.
     EXIT.
****************************************************************
*                       明細　印字                             *
****************************************************************
 MEISAI-SEC             SECTION.
     MOVE   WKJYO-F01          TO        JO-3.
     MOVE   WKJYO-F02          TO        JO-4.
     MOVE   WKJYO-F03          TO        JO-5.
     MOVE     SPACE            TO        DMY.
     MOVE   WKJYO-F04          TO        JO-6.
     MOVE   WKJYO-F05          TO        JO-7.
     MOVE   WKJYO-F06          TO        JO-8.
     MOVE   WKJYO-F07          TO        JO-9.
     MOVE   WKJYO-F08          TO        JO-10.
     MOVE   WKJYO-F09          TO        JO-11.
     MOVE   WKJYO-F10          TO        JO-12.
     MOVE   WKJYO-F11          TO        JO-13.
     MOVE   WKJYO-F12          TO        JO-14.
     MOVE   WKJYO-F12A         TO        JO-18.
     MOVE   WKJYO-F12B         TO        JO-19.
     MOVE   WKJYO-F12C         TO        JO-20.
     MOVE   WKJYO-F13          TO        JO-15.
     MOVE   WKJYO-F14          TO        JO-16.
     MOVE   WKJYO-F15          TO        JO-17.
*更新履歴
     EVALUATE    LOG-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   MEI-201.
*
     MOVE        LOG-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   MEI-202.
*担当者名取得
     PERFORM     READ-TAN-SEC.
*
     MOVE        TAN-F03                TO  HEN-TANNM.
     MOVE        HEN-TAN                TO  MEI-203.
     MOVE        LOG-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        LOG-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        LOG-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  MEI-204.
*
     MOVE        LOG-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        LOG-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        LOG-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  MEI-205.
*
     WRITE    PRTF-REC       FROM      MEISAI-1     AFTER   2.
     WRITE    PRTF-REC       FROM      MEISAI-2     AFTER   1.
     WRITE    PRTF-REC       FROM      MEISAI-21    AFTER   1.
 MEISAI-EXIT.
     EXIT.
****************************************************************
*                       見出し                                 *
****************************************************************
 MIDASI-SEC             SECTION.
*
     COMPUTE  CNT-PAGE   =   CNT-PAGE   +   1.
     MOVE     CNT-PAGE       TO        OMI-PAGE.
     WRITE    PRTF-REC       FROM      OMIDASHI     AFTER   2.
     WRITE    PRTF-REC  FROM    MIDASI-12  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-13  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-12  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-15  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-15  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-15  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-15  AFTER  1.
     WRITE    PRTF-REC  FROM    MIDASI-12  AFTER  1.
     WRITE    PRTF-REC       FROM      KOMIDASHI-1  AFTER   2.
     WRITE    PRTF-REC       FROM      KOMIDASHI-2  AFTER   1.
     WRITE    PRTF-REC  FROM    MIDASI-21  AFTER   1.
     MOVE        1           TO        MI-FLG    MSG-FLG.
 MIDASI-EXIT.
     EXIT.
************************************************************
*      2.2.1     履歴ファイル読込み処理                    *
************************************************************
 READ-LOG-SEC              SECTION.
*
     READ     MSTLOGF   NEXT
              AT END    MOVE  9      TO  END-FLG
              GO             TO   READ-LOG-END
     END-READ.
**
     ADD      1        TO   READ-CNT.
***  マスタ区分のチェック
     IF       LOG-F01  NOT =  "05"
              MOVE       9   TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  部門CD範囲のチェック
     IF       LOG-F02    >   PARA-BUMONCD
              MOVE       9   TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  担当者CD範囲のチェック
***##IF       LOG-F03(1:2)  =   PARA-TANCD
     IF       LOG-F03       =   PARA-TANCD
              CONTINUE
     ELSE
              MOVE       9   TO   END-FLG
              GO             TO   READ-LOG-END
     END-IF.
***  更新日範囲のチェック
     IF   ( LOG-F05  <=   PARA-UPDDATEE )  AND
          ( LOG-F06  <=   PARA-UPDTIMEE )
              CONTINUE
     ELSE
              GO             TO   READ-LOG-SEC
     END-IF.
**
     ADD    1          TO  IN-CNT.
     MOVE    LOG-F08   TO  WKJYO-REC.
**
 READ-LOG-END.
     EXIT.
************************************************************
*      2.2.2     担当者マスタ読込み処理                    *
************************************************************
 READ-TAN-SEC              SECTION.
*
     MOVE    LOG-F02       TO  TAN-F01.
     MOVE    LOG-F03       TO  TAN-F02.
     READ     HTANMS
         INVALID
              MOVE   ALL NC"＊" TO    TAN-F03
              MOVE   ALL "*"    TO    TAN-F04
     END-READ.
 READ-TAN-END.
     EXIT.
*    ENDCOBOL.
