****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成　                  *
*    作成日／更新日　　　：　93/06/23                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　直送データ抽出　                  *
*    OLD ID = ZMO0115B                                         *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0050B.
*AUTHER.                NAV.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
         STATION   IS   STA
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
***≪入庫ファイル≫*********************************************
     SELECT  ZNYUKDTF   ASSIGN    TO  DA-S-ZNYUKDT
                        ORGANIZATION  IS   SEQUENTIAL
                        ACCESS  MODE  IS   SEQUENTIAL
                        FILE  STATUS  IS   NYU-ST1.
***≪条件ファイル≫*********************************************
     SELECT  HJYOKEN    ASSIGN    TO  DA-VI-JYOKEN1
                        ORGANIZATION  IS   INDEXED
                        ACCESS  MODE  IS   RANDOM
                        RECORD  KEY   IS   JYO-F01
                                           JYO-F02
                        FILE  STATUS  IS   JYO-ST1.
***≪請求合計ファイル≫*****************************************
     SELECT  OUTF       ASSIGN    TO  DA-01-S-OUTF
                        FILE  STATUS  IS   OUT-ST1.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----≪入庫ファイル≫------------------------------------------*
 FD  ZNYUKDTF.
     COPY    ZNYUKDT    OF   XFDLIB    JOINING   NYU  PREFIX.
*----≪条件ファイル≫------------------------------------------*
 FD  HJYOKEN.
     COPY    JYOKEN1    OF   XFDLIB    JOINING   JYO  PREFIX.
*----≪請求合計ファイル≫--------------------------------------*
 FD  OUTF.
     COPY    ZNYUKDT    OF   XFDLIB    JOINING   OUT  PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
 01  PGM-ID             PIC   X(08)    VALUE     "SSE0050B".
*----≪フラグ≫------------------------------------------------*
 01  FLG.
     03  END-FLG        PIC   X(03)    VALUE     SPACE.
     03  A-FLG          PIC   9(01)    VALUE ZERO.
*----≪ワーク≫------------------------------------------------*
 01  WK-AREA.
     03  WK-DENKUBUN    PIC   9(02)    VALUE     ZERO.
     03  WK-HIZUKE      PIC   9(08)    VALUE     ZERO.
     03  WK-TOKCD       PIC   9(08)    VALUE     ZERO.
     03  WK-RYONO       PIC   9(09)    VALUE     ZERO.
     03  WK-SEIKIN      PIC  S9(09)    VALUE     ZERO.
     03  WK-SURYO       PIC  S9(09)V99 VALUE     ZERO.
     03  WK-KIN         PIC  S9(09)    VALUE     ZERO.
     03  WK-SUU         PIC  S9(09)V99 VALUE     ZERO.
*----≪ステータスエリア≫--------------------------------------*
 01  ST-AREA.
     03  IN-DATA        PIC   X(01)    VALUE     SPACE.
     03  NYU-ST1        PIC   X(02)    VALUE     SPACE.
     03  JYO-ST1        PIC   X(02)    VALUE     SPACE.
     03  OUT-ST1        PIC   X(02)    VALUE     SPACE.
*----≪システム日付　時間エリア≫------------------------------*
 01  SYS-DATE           PIC   9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC   9(02).
     03  SYS-MM         PIC   9(02).
     03  SYS-DD         PIC   9(02).
 01  SYS-TIME.
     03  SYS-HH         PIC   9(02).
     03  SYS-MN         PIC   9(02).
     03  SYS-SS         PIC   9(02).
*----≪メッセージエリア≫--------------------------------------*
 01  IN-MSG.
     03  FILLER         PIC   X(26)    VALUE
         "ﾆｭｳｺﾌｧｲﾙ      (IN)  =".
     03  CNT-IN         PIC   ZZZZZZ9.
 01  SKIP-MSG.
     03  FILLER         PIC   X(26)    VALUE
         "ﾆｭｳｺﾌｧｲﾙ    (SKIP)  =".
     03  CNT-SKIP       PIC   ZZZZZZ9.
 01  OUT-MSG.
     03  FILLER         PIC   X(26)    VALUE
         "ﾆｭｳｺﾌｧｲﾙ     (OUT)  =".
     03  CNT-OUT        PIC   ZZZZZZ9.
 01  WR-MSG.
     03  FILLER         PIC   X(26)    VALUE
         "OUT       ﾌｧｲﾙ(WR)  =".
     03  CNT-WR         PIC   ZZZZZZ9.
*----≪カウント　エリア≫--------------------------------------*
 01  CNT-AREA.
     03  IN-CNT         PIC   9(07)    VALUE ZERO.
     03  SKIP-CNT       PIC   9(07)    VALUE ZERO.
     03  OUT-CNT        PIC   9(07)    VALUE ZERO.
     03  WR-CNT         PIC   9(07)    VALUE ZERO.
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
 DECLARATIVES.
*----入庫ファイル----------------------------------------------*
 NYU-ERR                SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE      ZNYUKDTF.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"入庫ファイル異常！"
              "ST1=" NYU-ST1                  " "
               SYS-YY "." SYS-MM "." SYS-DD " "
               SYS-HH "." SYS-MN "." SYS-SS " ###"  UPON  STA.
     ACCEPT    IN-DATA  FROM STA.
     STOP      RUN.
*----条件ファイル----------------------------------------------*
 JYO-ERR                SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE      HJYOKEN.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                  " "
               SYS-YY "." SYS-MM "." SYS-DD " "
               SYS-HH "." SYS-MN "." SYS-SS " ###"  UPON  STA.
     ACCEPT    IN-DATA  FROM STA.
     STOP      RUN.
*----請求合計ファイル------------------------------------------*
 OUT-ERR                SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE      OUTF.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"アウト　ファイル異常！"
              "ST1=" OUT-ST1                  " "
               SYS-YY "." SYS-MM "." SYS-DD " "
               SYS-HH "." SYS-MN "." SYS-SS " ###"  UPON  STA.
     ACCEPT    IN-DATA  FROM STA.
     STOP      RUN.
 END DECLARATIVES.
****************************************************************
*        0.0    プログラムコントロール　　　　　　　　　　　   *
****************************************************************
 PROG-CNTL-RTN          SECTION.
     PERFORM            SYOKI-RTN.
     PERFORM            MAIN-RTN
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-RTN.
     STOP      RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*        1.0    初期処理　　　　　　　　　　　　　　　　　　   *
****************************************************************
 SYOKI-RTN              SECTION.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "***  " PGM-ID " START  "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH "." SYS-MN "." SYS-SS " ***"  UPON  STA.
***ファイルＯＰＥＮ
     OPEN     I-O       ZNYUKDTF.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    OUTF.
***条件ファイルＲＥＡＤ
     PERFORM            JYO-READ-RTN.
***入庫ファイルＲＥＡＤ
     PERFORM            NYU-READ-RTN.
 SYOKI-EXIT.
     EXIT.
****************************************************************
*        2.0    メイン処理　　　　　　　　　　　　　　　　　   *
****************************************************************
 MAIN-RTN               SECTION.
*
     IF     ( NYU-F06        =    ZERO     ) OR
            ( NYU-F33   NOT  =    SPACE    )
              GO TO               MAIN-99
     END-IF.
*
     MOVE     NYU-REC        TO   OUT-REC.
     WRITE                        OUT-REC.
     ADD      1              TO   OUT-CNT.
     MOVE     "1"            TO   NYU-F33.
     REWRITE                      NYU-REC.
*
 MAIN-99.
     PERFORM  NYU-READ-RTN.
 MAIN-EXIT.
     EXIT.
****************************************************************
*        3.0    終了処理                                       *
****************************************************************
 END-RTN                SECTION.
***ファイルＣＬＯＳＥ
     CLOSE    ZNYUKDTF   HJYOKEN   OUTF.
     MOVE     IN-CNT    TO     CNT-IN.
     MOVE     SKIP-CNT  TO     CNT-SKIP.
     MOVE     OUT-CNT   TO     CNT-OUT.
     MOVE     WR-CNT    TO     CNT-WR.
     DISPLAY  IN-MSG    UPON   CONS.
     DISPLAY  SKIP-MSG  UPON   CONS.
     DISPLAY  OUT-MSG   UPON   CONS.
     DISPLAY  WR-MSG    UPON   CONS.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "***  " PGM-ID " END    "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH "." SYS-MN "." SYS-SS " ***"  UPON  STA.
 END-EXIT.
     EXIT.
****************************************************************
*        1.1    条件ファイルＲＥＡＤ処理                       *
****************************************************************
 JYO-READ-RTN           SECTION.
     MOVE     59             TO    JYO-F01.
     MOVE     SPACE          TO    JYO-F02.
     READ     HJYOKEN
              INVALID   KEY
                   DISPLAY   "HJYOKEN READ ERROR" UPON CONS
                   MOVE      "END"    TO   END-FLG
              NOT  INVALID   KEY
                   MOVE      JYO-F04   TO  WK-HIZUKE
     END-READ.
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*        1.2    入庫ファイル　ＲＥＡＤ処理                     *
****************************************************************
 NYU-READ-RTN           SECTION.
     READ     ZNYUKDTF
              AT END    MOVE      "END"     TO   END-FLG
              GO   TO   NYU-READ-EXIT
     END-READ.
     ADD      1         TO    IN-CNT.
     IF     ( NYU-F01   =  60 OR 61  ) AND
            ( NYU-F97   =  SPACE     ) AND
            ( NYU-F06   NOT =  ZERO  ) AND
            ( NYU-F28   =  1         ) AND
            ( NYU-F23   <= WK-HIZUKE ) AND
            ( NYU-F33   =  SPACE     )
       THEN
              CONTINUE
        ELSE
              ADD  1    TO    SKIP-CNT
              GO   TO   NYU-READ-RTN
     END-IF.
 NYU-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***************************
