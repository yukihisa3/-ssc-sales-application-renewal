# SKZ0056B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKZ0056B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　出荷依頼リスト発行指示            *
*    作成日／更新日　　　：　2011/05/30                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　自動出力用の出荷依頼リスト発行指　*
*                            示を行う。　　　　　　　　　　　  *
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKZ0056B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          11/05/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*****************************************************************
 WORKING-STORAGE        SECTION.
*   <日付管理>
 01  WK-DATE                 PIC   9(06)     VALUE  ZERO.
 01  WK-SYS-DATE             PIC   9(08)     VALUE  ZERO.
 01  WK-NOU-DATE-1           PIC   9(08)     VALUE  ZERO.
 01  WK-NOU-DATE-2           PIC   9(08)     VALUE  ZERO.
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-DSOKO             PIC   X(02).
 01  PARA-SNOUDT            PIC   9(08).
 01  PARA-ENOUDT            PIC   9(08).
 01  PARA-SSAKUSEI          PIC   9(08).
 01  PARA-ESAKUSEI          PIC   9(08).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-DTKBN             PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-BUMON
                                       PARA-SOKO
                                       PARA-DSOKO
                                       PARA-SNOUDT
                                       PARA-ENOUDT
                                       PARA-SSAKUSEI
                                       PARA-ESAKUSEI
                                       PARA-TORICD
                                       PARA-TANCD
                                       PARA-DTKBN.
 DECLARATIVES.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
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
     MOVE      LINK-OUT-YMD       TO   WK-SYS-DATE.
*＋１日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        1            TO     LINK-IN-YMD6.
     MOVE        WK-SYS-DATE  TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE        LINK-OUT-YMD TO     WK-NOU-DATE-1.
*＋３日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        3            TO    LINK-IN-YMD6.
     MOVE        WK-NOU-DATE-1 TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE        LINK-OUT-YMD TO     WK-NOU-DATE-2.
*パラメタセット
*<部門>
*<倉庫>
     MOVE        SPACE        TO     PARA-SOKO.
*<代表倉庫>
*<納品日>
     MOVE    WK-NOU-DATE-1    TO     PARA-SNOUDT.
     MOVE    WK-NOU-DATE-2    TO     PARA-ENOUDT.
*<作成日>
     MOVE    ZERO             TO     PARA-SSAKUSEI.
     MOVE    99999999         TO     PARA-ESAKUSEI.
*<取引先ＣＤ>
     MOVE    ZERO             TO     PARA-TORICD.
*<担当者ＣＤ>
     MOVE    SPACE            TO     PARA-TANCD.
*<データ区分>
     MOVE    SPACE            TO     PARA-DTKBN.
*
     DISPLAY "PARA-BUMON    = " PARA-BUMON     UPON CONS.
     DISPLAY "PARA-SOKO     = " PARA-SOKO      UPON CONS.
     DISPLAY "PARA-DSOKO    = " PARA-DSOKO     UPON CONS.
     DISPLAY "PARA-SNOUDT   = " PARA-SNOUDT    UPON CONS.
     DISPLAY "PARA-ENOUDT   = " PARA-ENOUDT    UPON CONS.
     DISPLAY "PARA-SSAKUSEI = " PARA-SSAKUSEI  UPON CONS.
     DISPLAY "PARA-ESAKUSEI = " PARA-ESAKUSEI  UPON CONS.
     DISPLAY "PARA-TOTICD   = " PARA-TORICD    UPON CONS.
     DISPLAY "PARA-TANCD    = " PARA-TANCD     UPON CONS.
     DISPLAY "PARA-DTKBN    = " PARA-DTKBN     UPON CONS.
*
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
