****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ヨドバシＥＤＩ　　　　　　　      *
*    業務名　　　　　　　：　受注　　　　　　　　　            *
*    モジュール名　　　　：　ヨドバシ基本情報ファイル消込　　　*
*    作成日／作成者　　　：　2021/08/18 INOUE                  *
*    処理概要　　　　　　：　条件Ｆを読み、データ削除基準月数　*
*                            を取得→削除基準日を算出。　　　　*
*                            納品期日が合致するレコードを　　　*
*                            物理削除する。　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY9409B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  基本情報ファイル  >>---*
     SELECT   YODJOHL3   ASSIGN    TO             YODJOHL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   YOD-F14
                                                 YOD-F17
                                                 YOD-F03
                                                 YOD-F02
                                                 YOD-F07
                        FILE      STATUS    IS   YOD-STATUS.
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
*---<<  基本情報ファイル  >>---*
 FD  YODJOHL3
                        LABEL     RECORD    IS   STANDARD.
                        COPY      YODJOHL3  OF   XFDLIB
                        JOINING   YOD       AS   PREFIX.
*---<<  条件ファイル   >>---*
 FD  HJYOKEN
                        LABEL     RECORD    IS   STANDARD.
                        COPY      HJYOKEN   OF   XFDLIB
                        JOINING   JYO       AS   PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  YOD-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  READ-CNT            PIC  9(07)  VALUE ZERO.
     03  DEL-CNT             PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  ***
 01  DATE-AREA.
     03  WK-DATE             PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE            PIC  9(08).
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
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SSY9409B".
       05  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*       0.0   エラー処理                                      *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    YODJOHL3.
     MOVE     "YODJOHL3"     TO   ERR-FL-ID.
     MOVE     YOD-STATUS     TO   ERR-STCD.
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
*ファイルのＯＰＥＮ
     OPEN    I-O             YODJOHL3.
     OPEN    INPUT           HJYOKEN.
*在庫締日取得
     MOVE    99           TO JYO-F01.
     MOVE   "ZAI    "     TO JYO-F02.
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
     MOVE   "NAFUDEL"     TO JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY
         NC"＃＃　条件ファイル取得エラー（削除基準月数）　＃＃"
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
*基本情報ファイル読込み
     PERFORM YOD-READ-SEC.
     IF      "END"     =  END-FLG
             DISPLAY
             NC"＃＃　削除対象データ無し！！　２　＃＃"
                                          UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
*-------------------------------------------------------------*
*      基本情報ファイル読込                                   *
*-------------------------------------------------------------*
 YOD-READ-SEC                SECTION.
*
 YOD-READ-010.
     READ    YODJOHL3
             AT  END
             MOVE   "END"      TO   END-FLG
             GO                TO   YOD-READ-EXIT
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
 YOD-READ-020.
*経過件数表示
     IF      READ-CNT(5:3)     =    "000"  OR  "500"
             DISPLAY "READ-CNT =    " READ-CNT UPON CONS
     END-IF.
 YOD-READ-030.
*基本情報ファイルの納品期日が基準日より大きい場合
*****DISPLAY "WK-DEL-DATE = " WK-DEL-DATE  UPON CONS.
*****DISPLAY "YOD-F14     = " YOD-F14      UPON CONS.
     IF      YOD-F14  >  WK-DEL-DATE
             MOVE   "END"      TO   END-FLG
     END-IF.
*
 YOD-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      3.0　　メイン処理                                      *
*-------------------------------------------------------------*
 MAIN-SEC                    SECTION.
*基本情報ファイル削除
     DELETE   YODJOHL3.
     ADD      1            TO     DEL-CNT.
*基本情報ファイル読込み
     PERFORM YOD-READ-SEC.
*
 MAIN-END.
     EXIT.
*-------------------------------------------------------------*
*      4.0        終了処理                                    *
*-------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE              YODJOHL3  HJYOKEN.
     DISPLAY  "DELDATE          = "  WK-DEL-DATE   UPON   CONS.
     DISPLAY  "YODJOHL3    (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "YODJOHL3   (DEL) = "  DEL-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
