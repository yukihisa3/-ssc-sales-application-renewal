# SKZ0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKZ0110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　出荷状況管理Ｆ削除ＦＬＧセット    *
*    作成日／更新日　　　：　2011/04/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　削除ＦＬＧ＝空白でスタートし、出　*
*                            荷完了・送信完了が空白以外で、納　*
*                            品日＜＝システム日付のレコードの  *
*                            削除区分と日付をセットする。　　  *
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKZ0110B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/04/19.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷状況管理ファイル >>---*
     SELECT   SYUJISF   ASSIGN         DA-01-VI-SYUJISL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JIS-F95  JIS-F01
                                       JIS-F02  JIS-F03
                                       JIS-F04  JIS-F05
                                       JIS-F06
                        STATUS         JIS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*---<<  出荷状況管理ファイル >>---*
 FD  SYUJISF            LABEL RECORD   IS   STANDARD.
     COPY        SYUJISF     OF      XFDLIB
                 JOINING     JIS     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  DEL-CNT        PIC  9(07).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SAKUJO          PIC  9(02)     VALUE  ZERO.
 01  WK-HENKAN          PIC  9(06)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JIS-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-ZAIKO-DATE.
     03  WK-ZAI-1.
         05  WK-ZAI-11  PIC  9(04)     VALUE  ZERO.
         05  WK-ZAI-12  PIC  9(02)     VALUE  ZERO.
     03  WK-ZAI-2       PIC  9(02)     VALUE  ZERO.
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*---<<  出荷状況管理ファイル >>---*
 JIS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUJISF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKZ0110B SYUJISL4   ERROR " JIS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     I-O       SYUJISF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷状況管理ファイル読込
     PERFORM  SYUJISF-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    削除ＦＬＧセット
     MOVE    "1"         TO       JIS-F95.
     MOVE    WK-SYS-DATE TO       JIS-F96.
     REWRITE JIS-REC.
     ADD      1          TO       DEL-CNT.
*    出荷情報読込み
     PERFORM  SYUJISF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SYUJISF.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELSET CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKZ0110B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SYUJISF-READ-SEC       SECTION.
*
     READ     SYUJISF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   SYUJISF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    読込み件数表示
     IF       IN-CNT(5:3)  =  "000"  OR  "500"
            DISPLAY "READ-CNT = " IN-CNT " : " DEL-CNT UPON CONS
     END-IF.
*    削除FLGチェック
     IF       JIS-F95  =  "1"
              MOVE  "END"    TO   END-FLG
              GO             TO   SYUJISF-READ-EXIT
     END-IF.
*    出荷担当者チェック
     IF       JIS-F11  NOT =  SPACE
     AND      JIS-F13  NOT =  SPACE
              CONTINUE
     ELSE
              GO             TO   SYUJISF-READ-SEC
     END-IF.
*
 SYUJISF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
