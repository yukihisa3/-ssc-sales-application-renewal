# SBT9000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT9000B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携（カインズ）　　　　*
*    モジュール名　　　　：　カインズ出荷連携累積ファイル消込　*
*    作成日／作成者　　　：　2014/08/14                        *
*    処理概要　　　　　　：　条件Ｆより累積データ削除基準月数　*
*                            を取得→削除基準日を算出          *
*                            →最終納品先納品日が基準日以前　  *
*                            　の累積レコードを削除する。　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SBT9000B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷連携累積データ  >>---*
     SELECT   CNZSYRL3  ASSIGN    TO             CNZSYRL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   CNZ-F346
                        FILE      STATUS    IS   CNZ-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO             JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  出荷連携累積データ  >>---*
 FD  CNZSYRL3
                        LABEL     RECORD    IS   STANDARD.
                        COPY      CNZSYRL3   OF   XFDLIB
                        JOINING   CNZ       AS   PREFIX.
*---<<  条件ファイル   >>---*
 FD  HJYOKEN
                        LABEL     RECORD    IS   STANDARD.
                        COPY      HJYOKEN   OF   XFDLIB
                        JOINING   JYO       AS   PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  CNZ-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  READ-CNT            PIC  9(07)  VALUE ZERO.
     03  DEL-CNT             PIC  9(07)  VALUE ZERO.
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
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SBT9000B".
       05  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*       0.0   エラー処理                                      *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    CNZSYRL3.
     MOVE     "CNZSYRL3"     TO   ERR-FL-ID.
     MOVE     CNZ-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
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
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
*-------------------------------------------------------------*
*       2.0   初期処理                                        *
*-------------------------------------------------------------*
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             CNZSYRL3.
     OPEN    INPUT           HJYOKEN.
*在庫締日取得
     MOVE    99           TO JYO-F01.
     MOVE   "ZAI"         TO JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY
         NC"＃＃　条件ファイル　取得エラー（在庫締め日）　＃＃"
                                          UPON CONS
             MOVE  4001   TO PROGRAM-STATUS
             STOP  RUN
     END-READ.
     MOVE    JYO-F05      TO WK-SIME.
*削除基準月数取得
     MOVE    99           TO JYO-F01.
     MOVE   "BMSDEL"      TO JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY
         NC"＃＃　条件ファイル　取得エラー（削除基準日）　＃＃"
                                           UPON CONS
             MOVE  4001   TO PROGRAM-STATUS
             STOP  RUN
     END-READ.
*削除基準日算出
     MOVE    JYO-F04      TO WK-KIKAN.
     MOVE    WK-SIME(1:4) TO WRK-DATE1R1.
     MOVE    WK-SIME(5:2) TO WRK-DATE1R2.
     DIVIDE   WK-KIKAN    BY    12   GIVING    WK-CHK01
                                     REMAINDER WK-CHK02.
     COMPUTE DEL-DATE1-1  =  WRK-DATE1R1 - WK-CHK01.
     COMPUTE WK-TUKI      =  WRK-DATE1R2 - WK-CHK02.
     IF      WK-TUKI      <  ZERO
             ADD  -1      TO DEL-DATE1-1
             COMPUTE  WK-TUKI     = WK-TUKI * -1
             COMPUTE  DEL-DATE1-2 = 12 - WK-TUKI
     ELSE
             MOVE WK-TUKI TO DEL-DATE1-2
     END-IF.
     MOVE    99           TO DEL-DATE2.
     DISPLAY "WK-DEL-DATE = " WK-DEL-DATE UPON CONS.
*出荷連携累積Ｆ読込み
     PERFORM CNZ-READ-SEC.
     IF      "END"     =  END-FLG
             DISPLAY
             NC"＃＃　出荷連携累積データ削除対象無　＃＃"
                                          UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
*-------------------------------------------------------------*
*      出荷連携累積Ｆ読込み                                   *
*-------------------------------------------------------------*
 CNZ-READ-SEC                SECTION.
*
     READ    CNZSYRL3
             AT  END
             MOVE   "END"      TO   END-FLG
             GO                TO   CNZ-READ-EXIT
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*経過件数表示
     IF      READ-CNT(5:3)     =    "000"  OR  "500"
             DISPLAY "READ-CNT =    " READ-CNT UPON CONS
     END-IF.
*出荷連携累積データの最終納品先納品日が
*削除基準日より大きい場合
     IF      CNZ-F346 >  WK-DEL-DATE
             MOVE   "END"      TO   END-FLG
     END-IF.
*
 CNZ-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      3.0　　メイン処理                                      *
*-------------------------------------------------------------*
 MAIN-SEC                    SECTION.
*出荷連携累積Ｆ削除
     DELETE   CNZSYRL3.
     ADD      1            TO     DEL-CNT.
*出荷連携累積Ｆ読込み
     PERFORM CNZ-READ-SEC.
*
 MAIN-END.
     EXIT.
*-------------------------------------------------------------*
*      4.0        終了処理                                    *
*-------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE              CNZSYRL3  HJYOKEN.
     DISPLAY  "DELDATE         = "  WK-DEL-DATE   UPON   CONS.
     DISPLAY  "CNZSYRL3   (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "CNZSYRL3  (DEL) = "  DEL-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
