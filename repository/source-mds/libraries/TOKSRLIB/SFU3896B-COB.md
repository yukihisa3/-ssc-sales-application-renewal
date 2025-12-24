# SFU3896B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3896B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替　　　　　　　　　　　*
*    モジュール名　　　　：　ＡＣＯＳ振替累積データ消込　      *
*    作成日／作成者　　　：　2016/12/21  INOUE                 *
*    処理概要　　　　　　：　規定保存期間を経過した社内振替　　*
*                            データの削除　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SFU3896B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＡＣＯＳ振替ＤＴ累積Ｆ  >>---*
     SELECT   FRACOSL1  ASSIGN    TO             FRACOSL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   FRA-F01
                                                 FRA-F03
                                                 FRA-F04
                                                 FRA-F08
                                                 FRA-F05
                                                 FRA-F07
                                                 FRA-F06
                        FILE      STATUS    IS   FRA-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＡＣＯＳ振替ＤＴ累積Ｆ  >>---*
 FD  FRACOSL1
                        LABEL     RECORD    IS   STANDARD.
                        COPY      FRACOSL1  OF   XFDLIB
                        JOINING   FRA       AS   PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  FRA-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  FRA-END-FLG         PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  FRA-READ-CNT        PIC  9(07)  VALUE ZERO.
     03  FRA-DEL-CNT         PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     03  WRK-DATE1           PIC  9(06).
     03  WRK-DATE1R          REDEFINES   WRK-DATE1.
         05  WRK-DATE1R1     PIC  9(04).
         05  WRK-DATE1R2     PIC  9(02).
     03  WRK-DATE2           PIC  9(06).
 01  WK-SIME                 PIC  9(06)  VALUE ZERO.
 01  WK-KIKAN                PIC  9(02)  VALUE ZERO.
 01  WK-TUKI                 PIC S9(02)  VALUE ZERO.
 01  WK-DEL-DATE.
     03  DEL-DATE1.
         05  DEL-DATE1-1     PIC  9(04)  VALUE ZERO.
         05  DEL-DATE1-2     PIC  9(02)  VALUE ZERO.
     03  DEL-DATE2           PIC  9(02)  VALUE ZERO.
 01  WK-CHK01                PIC  9(02)  VALUE ZERO.
 01  WK-CHK02                PIC  9(02)  VALUE ZERO.
 01  WK-FRA-F01              PIC  9(08)  VALUE ZERO.
*
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SFU3896B".
       05  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE  " ###".
*
 LINKAGE                SECTION.
 01  LINK-IN-DEL-DATE      PIC   9(08).
*
*-------------------------------------------------------------*
*       0.0   エラー処理                                      *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION
                             USING LINK-IN-DEL-DATE.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FRACOSL1.
     MOVE     "FRACOSL1"     TO   ERR-FL-ID.
     MOVE     FRA-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*-------------------------------------------------------------*
*       1.0   コントロール                                    *
*-------------------------------------------------------------*
 CONTROL-SEC                 SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  FRA-END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
*-------------------------------------------------------------*
*       2.0   初期処理                                        *
*-------------------------------------------------------------*
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             FRACOSL1.
*
     DISPLAY LINK-IN-DEL-DATE NC"以前を削除します" UPON CONS
*
*ファイルＳＴＡＲＴ
     PERFORM FRA-START-SEC.
     IF      "END"     =  FRA-END-FLG
             DISPLAY
             NC"＃＃　ＡＣＯＳ振替情報　削除対象無し　＃＃"
                                          UPON CONS
             GO     TO    INIT-END
     END-IF.
*
*ファイル読込み
     PERFORM FRA-READ-SEC.
     IF      "END"     =  FRA-END-FLG
             DISPLAY
             NC"＃＃　ＡＣＯＳ振替情報　削除対象無し　＃＃"
                                          UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
*-------------------------------------------------------------*
*      ファイルＳＴＡＲＴ                                     *
*-------------------------------------------------------------*
 FRA-START-SEC                SECTION.
*
     MOVE    ZERO              TO   FRA-F01.
     MOVE    SPACE             TO   FRA-F03.
     MOVE    SPACE             TO   FRA-F04.
     MOVE    ZERO              TO   FRA-F08.
     MOVE    SPACE             TO   FRA-F05.
     MOVE    ZERO              TO   FRA-F07.
     MOVE    SPACE             TO   FRA-F06.
     START   FRACOSL1   KEY IS >=   FRA-F01  FRA-F03  FRA-F04
                                    FRA-F08  FRA-F05  FRA-F07
                                    FRA-F06
             INVALID
             MOVE   "END"      TO   FRA-END-FLG
             GO                TO   FRA-START-EXIT
     END-START.
*
 FRA-START-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      ファイル読込                                           *
*-------------------------------------------------------------*
 FRA-READ-SEC                SECTION.
*
     READ    FRACOSL1
             AT  END
             MOVE   "END"      TO   FRA-END-FLG
             GO                TO   FRA-READ-EXIT
     END-READ.
*振替受信日が削除基準日を超えたら終了
     MOVE    FRA-F01           TO   WK-FRA-F01.
     IF      WK-FRA-F01    >   LINK-IN-DEL-DATE
             MOVE   "END"      TO   FRA-END-FLG
             GO                TO   FRA-READ-EXIT
     END-IF.
*経過件数表示
     ADD     1                 TO     FRA-READ-CNT.
     IF      FRA-READ-CNT(5:3) =    "000"  OR  "500"
             DISPLAY "READ-CNT =    " FRA-READ-CNT UPON CONS
     END-IF.
*
 FRA-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      3.0　　メイン処理                                      *
*-------------------------------------------------------------*
 MAIN-SEC                    SECTION.
*
 MAIN-01.
*ファイル削除
     DELETE   FRACOSL1.
     ADD      1            TO     FRA-DEL-CNT.
*
 MAIN-02.
*ファイル読込み
     PERFORM FRA-READ-SEC.
*
 MAIN-END.
     EXIT.
*-------------------------------------------------------------*
*      4.0        終了処理                                    *
*-------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE              FRACOSL1.
     DISPLAY  NC"削除基準日　　　＝" LINK-IN-DEL-DATE UPON CONS.
     DISPLAY  NC"累積データ　ＩＮ＝" FRA-READ-CNT     UPON CONS.
     DISPLAY  NC"　　　　　　削除＝" FRA-DEL-CNT      UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
