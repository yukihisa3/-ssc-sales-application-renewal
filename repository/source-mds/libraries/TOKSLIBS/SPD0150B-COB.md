# SPD0150B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD0150B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　在庫管理システム　　　　          *
*　　モジュール名　　　　：　（手書取込データ）量販店ＪＮＬ削除*
*　　作成日／更新日　　　：　2009/10/29                        *
*　　作成者／更新者　　　：　大野                              *
*　　処理概要　　　　　　：　パラメタより、取引先ＣＤとメモ_の*
*                  開始・終了を受取、範囲内の量販店ＪＮＬ（ＰＣ*
*                  用）を削除する　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SPD0150B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/10/29.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  量販店ＪＮＬ（ＰＣ連携用）  >>---*
     SELECT   PCRYOJF   ASSIGN         DA-01-VI-PCRYOJL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  PCR-F11
                                       PCR-F011
                                       PCR-F012
                                       PCR-F02
                        STATUS         PCR-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 量販店ＪＮＬ（ＰＣ連携用）>>--*
 FD  PCRYOJF            LABEL RECORD   IS   STANDARD.
     COPY        PCRYOJL4    OF      XFDLIB
                 JOINING     PCR     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  DEL-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SAKUJO          PIC  9(02)     VALUE  ZERO.
 01  WK-HENKAN          PIC  9(06)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  PCR-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
*
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
 LINKAGE                          SECTION.
 01  PARA-TORICD        PIC  9(08).
 01  PARA-MEMOST        PIC  9(04).
 01  PARA-MEMOEN        PIC  9(04).
***********************************************************
***********************************************************
 PROCEDURE          DIVISION  USING    PARA-TORICD  PARA-MEMOST
                                                    PARA-MEMOEN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷情報データ >>--*
 PCR-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCRYOJF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SPD0150B PCRYOJF    ERROR " PCR-ST " "
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
     OPEN     I-O       PCRYOJF.
*
*----<< 削除範囲出力 >>-*
     DISPLAY "削除範囲 = "  PARA-MEMOST "～" PARA-MEMOEN
                                             UPON CONS.
*ワークエリア　クリア
     INITIALIZE                COUNTERS.
     INITIALIZE                FLAGS.
*
*パラメタの開始メモ_でスタートを掛ける
*
     MOVE       PARA-TORICD    TO      PCR-F11.
     MOVE       PARA-MEMOST    TO      PCR-F011.
     MOVE       ZERO           TO      PCR-F012  PCR-F02.
*
     START    PCRYOJF   KEY  >=   PCR-F11   PCR-F011
                                  PCR-F012  PCR-F02
              INVALID
                   MOVE     "END"      TO      END-FLG
                   DISPLAY  "対象データなし"   UPON    CONS
                   GO                  TO      INIT-RTN-EXIT
     END-START.
*
     PERFORM  PCRYOJF-READ-SEC.
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    量販店ＪＮＬ削除
*
     IF  PCR-F011  >=   PARA-MEMOST
         DELETE   PCRYOJF
         ADD      1          TO       DEL-CNT
     END-IF.
*
*    量販店ＪＮＬ読込み
     PERFORM  PCRYOJF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    PCRYOJF.
*
     DISPLAY  "## READ   CNT => " IN-CNT    UPON CONS.
     DISPLAY  "## DELETE CNT => " DEL-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SPD0150B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　量販店ＪＮＬ読込み　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PCRYOJF-READ-SEC       SECTION.
*    量販店ＪＮＬ参照
     READ     PCRYOJF   NEXT AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   PCRYOJF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    メモ_チェック
     IF       PCR-F011 >  PARA-MEMOEN
              MOVE  "END"    TO   END-FLG
              GO             TO   PCRYOJF-READ-EXIT
     END-IF.
*
 PCRYOJF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
