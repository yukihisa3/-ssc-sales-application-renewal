# SJS0090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJS0090B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　実績管理システム　　　　　　　　　*
*    モジュール名　　　　：　計上データエラー分戻し処理        *
*    作成日／更新日　　　：　2000/06/28                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　計上データのエラー分より実績累積Ｆ*
*                            ・累積集計Ｆの戻し処理を行う。    *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0090B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＴＯＫＵファイル  >>---*
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  実績累積ファイル  >>---*
     SELECT   RUISEKF   ASSIGN    TO        DA-01-VI-RUISEKF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   RUI-F01
                                                 RUI-F06
                                                 RUI-F03
                                                 RUI-F05
                                                 RUI-F02
                                                 RUI-F04
                        FILE      STATUS    IS   RUI-STATUS.
*---<<  実績集計ファイル  >>---*
     SELECT     JISSYUF      ASSIGN    TO        DA-01-VI-JISSYUL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       JIS-F01
                                                 JIS-F021
                                                 JIS-F022
                                                 JIS-F03
                                                 JIS-F04
                                                 JIS-F051
                                                 JIS-F052
                             FILE      STATUS    JIS-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD  TOKU     BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     TOKU      OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  実績累積Ｆ  >>---*
 FD  RUISEKF.
     COPY     RUISEKF   OF        XFDLIB
              JOINING   RUI       PREFIX.
*---<<  実績集計ファイル  >>---*
 FD  JISSYUF.
     COPY     JISSYUF   OF        XFDLIB
              JOINING   JIS       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TOK-STATUS          PIC  X(02).
     02  RUI-STATUS          PIC  X(02).
     02  JIS-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  X(01)  VALUE SPACE.
     02  RUI-INV-FLG         PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  CRT-CNT             PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE ZERO.
     02  RDEL-CNT            PIC  9(07)  VALUE ZERO.
     02  JDEL-CNT            PIC  9(07)  VALUE ZERO.
****  計算ワーク
 01  KEISAN-WORK.
     03  WK-SUU              PIC S9(07)V99 VALUE  ZERO.
     03  WK-KIN              PIC S9(09)    VALUE  ZERO.
****  ＷＲＫ領域  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJS0090B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TOKU.
     MOVE     "TOKU"         TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    RUISEKF.
     MOVE     "RUISEKF"      TO   ERR-FL-ID.
     MOVE     RUI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JISSYUF.
     MOVE     "JISSYUF"      TO   ERR-FL-ID.
     MOVE     JIS-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    INPUT           TOKU.
     OPEN    I-O             RUISEKF   JISSYUF.
*振替データ初期読込み
     PERFORM TOKU-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      電算室殿⇒振替データファイル読込み                      *
****************************************************************
 TOKU-READ-SEC               SECTION.
*
     READ    TOKU
             AT  END
             MOVE   "END"      TO   END-FLG
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*
 TOKU-READ-EXIT.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*実績累積Ｆ存在チェック
     PERFORM  SONZAI-CHK-SEC.
*    存在確認
     IF    RUI-INV-FLG = "INV"
           CONTINUE
     ELSE
*          実績集計Ｆ戻し
           IF  TOK-F02  =  "50" OR "51" OR "60" OR "61" OR "70"
                        OR "71" OR "40" OR "41" OR "42" OR "45"
                        PERFORM   JISEKI-CHK-SEC
           END-IF
*          実績累積Ｆ戻し
           DELETE    RUISEKF
           ADD       1       TO   RDEL-CNT
     END-IF.
*売上計上Ｆ読込み
     PERFORM TOKU-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*            実績集計ファイル更新
****************************************************************
 JISEKI-CHK-SEC              SECTION.
*
     IF  RUI-F02 = "40" OR "41" OR "42" OR "45" OR "60" OR "61"
         MOVE      "1"       TO    JIS-F01
     ELSE
         MOVE      "2"       TO    JIS-F01
     END-IF.
     MOVE     RUI-F09(1:6)        TO   JIS-F02.
     MOVE     RUI-F06             TO   JIS-F03.
     MOVE     RUI-F17             TO   JIS-F04.
     MOVE     RUI-F10             TO   JIS-F051.
     MOVE     RUI-F11             TO   JIS-F052.
     READ     JISSYUF
         INVALID
              GO                  TO   JISEKI-CHK-EXIT
     END-READ.
*    通常
     IF  RUI-F02   =   "50" OR "70" OR "35" OR "40"
         IF   RUI-F05   =  "0" OR "2" OR "4" OR "6" OR "8"
              COMPUTE  JIS-F06  =  JIS-F06  -  RUI-F13
              COMPUTE  JIS-F07  =  JIS-F07  -  RUI-F15
         ELSE
              COMPUTE   WK-SUU    =    RUI-F13  *  -1
              COMPUTE   WK-KIN    =    RUI-F15  *  -1
              COMPUTE  JIS-F06  =  JIS-F06  -  WK-SUU
              COMPUTE  JIS-F07  =  JIS-F07  -  WK-KIN
         END-IF
     END-IF.
*    返品
     IF  RUI-F02   =   "51" OR "71" OR "36" OR "41"
         IF   RUI-F05   =  "0" OR "2" OR "4" OR "6" OR "8"
              COMPUTE  JIS-F08  =  JIS-F08  -  RUI-F13
              COMPUTE  JIS-F09  =  JIS-F09  -  RUI-F15
         ELSE
              COMPUTE   WK-SUU    =    RUI-F13  *  -1
              COMPUTE   WK-KIN    =    RUI-F15  *  -1
              COMPUTE  JIS-F08  =  JIS-F08  -  WK-SUU
              COMPUTE  JIS-F09  =  JIS-F09  -  WK-KIN
         END-IF
     END-IF.
*    値引
     IF  RUI-F02   =   "42"
         IF   RUI-F05   =  "0" OR "2" OR "4" OR "6" OR "8"
              COMPUTE  JIS-F10  =  JIS-F10  -  RUI-F15
         ELSE
              COMPUTE   WK-KIN    =    RUI-F15  *  -1
              COMPUTE  JIS-F10  =  JIS-F10  -  WK-KIN
         END-IF
     END-IF.
*    値増
     IF  RUI-F02   =   "45"
         IF   RUI-F05   =  "0" OR "2" OR "4" OR "6" OR "8"
              COMPUTE   WK-KIN         =    RUI-F15  * -1
              ADD       WK-KIN         TO   JIS-F10
              COMPUTE  JIS-F10  =  JIS-F10  -  WK-KIN
         ELSE
              ADD       RUI-F15        TO   JIS-F10
              COMPUTE  JIS-F10  =  JIS-F10  -  RUI-F15
         END-IF
     END-IF.
*実績集計Ｆ更新
     REWRITE  JIS-REC.
*戻し件数
     ADD      1         TO   JDEL-CNT.
*
 JISEKI-CHK-EXIT.
     EXIT.
****************************************************************
*    実績累積Ｆ存在チェック                                    *
****************************************************************
 SONZAI-CHK-SEC              SECTION.
*キーのセット
     MOVE   TOK-F01          TO    RUI-F01.
     MOVE   TOK-F06          TO    RUI-F06.
     MOVE   TOK-F03          TO    RUI-F03.
     MOVE   TOK-F05          TO    RUI-F05.
     MOVE   TOK-F02          TO    RUI-F02.
     MOVE   TOK-F04          TO    RUI-F04.
     READ   RUISEKF
            INVALID
            MOVE    "INV"    TO    RUI-INV-FLG
            NOT INVALID
            MOVE     SPACE   TO    RUI-INV-FLG
     END-READ.
*
 SONZAI-CHK-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKU  RUISEKF.
     DISPLAY  "ERR DATA   (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "RUISEKF   (DEL) = "  RDEL-CNT      UPON   CONS.
     DISPLAY  "JISSYUF   (DEL) = "  JDEL-CNT      UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
