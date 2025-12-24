# SKE0321B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0321B.COB`

## ソースコード

```cobol
****************************************************************
*    ※取引先ＣＤ＝０１７３専用                                *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　送信ＤＴ作成                      *
*　　作成日／更新日　　　：　2001/05/14                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　出荷情報Ｆ・梱包ラベルＦより、ケー*
*　　　　　　　　　　　　　　ヨー送信Ｆを作成する。　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0321B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/05/14.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RUI-F11
                                       RUI-F04
                                       RUI-F03
                                       RUI-F02
                                       RUI-F05
                                       RUI-F06
                                       RUI-F07
                        STATUS         RUI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷情報データ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  END-FLG2       PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  ONL1-CNT       PIC  9(06).
     03  ONL2-CNT       PIC  9(06).
     03  ONL3-CNT       PIC  9(06).
     03  ONL4-CNT       PIC  9(06).
     03  DEN-CNT        PIC  9(06).
     03  SOK-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-RENBAN          PIC  9(05)     VALUE  ZERO.
 01  IX                 PIC  9(01)     VALUE  ZERO.
 01  KENSOKF-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-CHK-AREA.
     03  NEW-TEN        PIC  9(05)     VALUE  ZERO.
     03  OLD-TEN        PIC  9(05)     VALUE  ZERO.
     03  NEW-KEN        PIC  9(08)     VALUE  ZERO.
     03  OLD-KEN        PIC  9(08)     VALUE  ZERO.
     03  NEW-TOK        PIC  9(04)     VALUE  ZERO.
     03  OLD-TOK        PIC  9(04)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUI-ST             PIC  X(02).
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
 01  WK-RUI.
     03  WK-RUI-F04     PIC  9(04)     VALUE  ZERO.
     03  WK-RUI-F03     PIC  9(08)     VALUE  ZERO.
     03  WK-RUI-F02     PIC  9(05)     VALUE  ZERO.
 01  WK-HED-TOKCD.
     03  WK-HED-01      PIC  X(02)     VALUE  SPACE.
     03  WK-HED-02      PIC  X(04)     VALUE  SPACE.
     03  WK-HED-03      PIC  X(02)     VALUE  SPACE.
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
*----<< 出荷情報データ >>--*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0321B RUISYUF    ERROR " RUI-ST " "
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
     OPEN     I-O       RUISYUF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     MOVE     ZERO      TO      WK-RENBAN.
*出荷情報データ初期読込み
     PERFORM  RUISYUF-READ-SEC.
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
*    配送店ヘッダ出力
     MOVE        1      TO          RUI-F11.
     REWRITE     RUI-REC.
*    出荷情報読込み
     PERFORM  RUISYUF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RUISYUF.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUISYUF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     RUISYUF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
     IF       IN-CNT(4:3)  =  "000"  OR  "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*    送信ＦＬＧチェック
     IF       RUI-F11  =  1
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-IF.
*    取引先制限
*取引先ＣＤが”０１７３”の時のみ対象
     IF       RUI-F04   >   173
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-IF.
*    取引先制限
     IF       RUI-F03   <   20020514
              CONTINUE
     ELSE
              GO             TO   RUISYUF-READ-SEC
     END-IF.
*
 RUISYUF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
