# SSE0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE0040B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成　                  *
*    作成日／更新日　　　：　93/06/23                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    コンパイラ無し                                            *
*    （入庫データ→直送データ請求Ｆ） OLD ID = ZMO0110B        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0040B.
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
     SELECT  ZNYUKDTF   ASSIGN    TO  DA-S-ZNYUKDTF
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
     SELECT  HSEIGKF    ASSIGN    TO  DA-VI-SEIGK1
                        ORGANIZATION  IS   INDEXED
                        ACCESS  MODE  IS   DYNAMIC
                        RECORD  KEY   IS   SEI-F01
                                           SEI-F05
                        FILE  STATUS  IS   SEI-ST1.
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
 FD  HSEIGKF            BLOCK   CONTAINS  40   RECORDS.
     COPY    SETGKFA    OF   XFDLIB    JOINING   SEI  PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
     COPY    SETGKFA    OF   XFDLIB    JOINING   SEIWK  PREFIX.
 01  PGM-ID             PIC   X(08)    VALUE     "SSE0040B".
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
     03  SEI-ST1        PIC   X(02)    VALUE     SPACE.
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
 01  INV-FLG            PIC   9(01)    VALUE  ZERO.
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
         "ｾｲｷｭｳｺﾞｳｹｲﾌｧｲﾙ(WR)  =".
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
 SEI-ERR                SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE      HSEIGKF.
     ACCEPT    SYS-DATE   FROM  DATE.
     ACCEPT    SYS-TIME   FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"請求合計ファイル異常！"
              "ST1=" SEI-ST1                  " "
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
     OPEN     INPUT     ZNYUKDTF  HJYOKEN.
     OPEN     I-O       HSEIGKF.
***条件ファイルＲＥＡＤ
     PERFORM            JYO-READ-RTN.
***入庫ファイルＲＥＡＤ
     PERFORM            NYU-READ-RTN.
***得意先コード／量販_  退避
     IF  END-FLG   NOT  =    "END"
         MOVE      NYU-F01   TO   WK-DENKUBUN
         MOVE      NYU-F08   TO   WK-TOKCD
         MOVE      NYU-F06   TO   WK-RYONO
     END-IF.
 SYOKI-EXIT.
     EXIT.
****************************************************************
*        2.0    メイン処理　　　　　　　　　　　　　　　　　   *
****************************************************************
 MAIN-RTN               SECTION.
***得意先コード／量販_ ブレイク判定
     IF     ( NYU-F08   NOT  =    WK-TOKCD ) OR
            ( NYU-F06   NOT  =    WK-RYONO )
***請求合計ファイルＷＲＩＴＥ処理
              PERFORM   SEI-WR-RTN
     END-IF.
***集計処理
     PERFORM  SEI-SYUUKEI-RTN.
***入庫ファイルＲＥＡＤ
     PERFORM  NYU-READ-RTN.
 MAIN-EXIT.
     EXIT.
****************************************************************
*        2.1    請求合計ファイル　ＷＲＩＴＥ処理               *
****************************************************************
 SEI-WR-RTN              SECTION.
*****MOVE     WK-DENKUBUN    TO   SEI-F07.
     IF       WK-DENKUBUN    =    60
         MOVE     40             TO   SEI-F07
     ELSE
         MOVE     41             TO   SEI-F07
     END-IF.
     MOVE     WK-SEIKIN      TO   SEI-F06.
     MOVE     WK-SURYO       TO   SEI-F09.
*
     MOVE     ZERO           TO   INV-FLG.
     READ     HSEIGKF   INTO      SEIWK-REC
           INVALID  KEY
              MOVE      1    TO   INV-FLG
     END-READ.
*
     IF       INV-FLG    =   1
              ADD      1    TO   WR-CNT
              WRITE    SEI-REC
     ELSE
              ADD            SEIWK-F06 TO SEI-F06
              MOVE           SEIWK-F01 TO SEI-F01
              MOVE           SEIWK-F02 TO SEI-F02
              MOVE           SEIWK-F03 TO SEI-F03
              MOVE           SEIWK-F04 TO SEI-F04
              MOVE           SEIWK-F05 TO SEI-F05
              MOVE           SEIWK-F09 TO SEI-F09
              MOVE           SEIWK-F07 TO SEI-F07
              MOVE           SEIWK-F07 TO SEI-F08
              MOVE           SEIWK-F10 TO SEI-F10
              MOVE           SEIWK-F11 TO SEI-F11
         IF   SEI-F06        NOT =     ZERO
              REWRITE        SEI-REC
         ELSE
              DELETE          HSEIGKF
         END-IF
     END-IF.

     MOVE     ZERO      TO   WK-SEIKIN.
     MOVE     ZERO      TO   WK-SURYO.
     MOVE     ZERO      TO   A-FLG.
***伝票区分退避
     MOVE     NYU-F01   TO   WK-DENKUBUN.
***得意先コード／量販_  退避
     MOVE     NYU-F08   TO   WK-TOKCD.
     MOVE     NYU-F06   TO   WK-RYONO.
 SEI-WR-EXIT.
     EXIT.
****************************************************************
*        2.2    集計処理　　　　　　　　　　　　　　　　　　   *
****************************************************************
 SEI-SYUUKEI-RTN         SECTION.
***明細移動
     IF  A-FLG     =    ZERO
         MOVE      SPACE     TO   SEI-REC
         INITIALIZE     SEI-REC
         MOVE      NYU-F08   TO   SEI-F01
         MOVE      WK-HIZUKE TO   SEI-F02
         MOVE      NYU-F12   TO   SEI-F03
         MOVE      NYU-F23   TO   SEI-F04
         MOVE      NYU-F06   TO   SEI-F05
         MOVE      ZERO      TO   SEI-F08
         MOVE      NYU-F23   TO   SEI-F10
         MOVE      NYU-F08   TO   SEI-F11
         MOVE      1         TO   A-FLG
     END-IF.
***伝票区分
     IF ( WK-DENKUBUN    =    61 )   AND
        ( NYU-F01        =    60 )
         MOVE      60   TO   WK-DENKUBUN
     END-IF.
***請求金額・数量　集計
     IF  ( NYU-F01  =  60 )  AND
         ( NYU-F04  =  1  OR  3  OR  5  OR  7  OR  9 )
           COMPUTE  WK-KIN    =  NYU-F19  *  -1   *  NYU-F15
           ADD      WK-KIN    TO     WK-SEIKIN
           COMPUTE  WK-SUU    =  NYU-F15  *  -1
           ADD      WK-SUU    TO     WK-SURYO
           GO       TO   SEI-SYUUKEI-EXIT
     END-IF.
     IF  ( NYU-F01  =  61 )  AND
         ( NYU-F04  =  0  OR  2  OR  4  OR  6  OR  8 )
           COMPUTE  WK-KIN    =  NYU-F19  *  -1   *  NYU-F15
           ADD      WK-KIN    TO     WK-SEIKIN
           COMPUTE  WK-SUU    =  NYU-F15  *  -1
           ADD      WK-SUU    TO     WK-SURYO
           GO       TO   SEI-SYUUKEI-EXIT
     END-IF.
     ADD   NYU-F15  TO  WK-SURYO.
*****ADD   NYU-F19  TO  WK-SEIKIN.
     COMPUTE  WK-KIN    =      NYU-F19  *  NYU-F15.
     ADD      WK-KIN    TO     WK-SEIKIN.
*
 SEI-SYUUKEI-EXIT.
     EXIT.
****************************************************************
*        3.0    終了処理                                       *
****************************************************************
 END-RTN                SECTION.
***請求合計ファイルＷＲＩＴＥ処理
     IF   A-FLG    NOT  =    ZERO
          PERFORM  SEI-WR-RTN
     END-IF.
***ファイルＣＬＯＳＥ
     CLOSE    ZNYUKDTF   HJYOKEN   HSEIGKF.
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
            ( NYU-F23  <=  WK-HIZUKE )
              ADD  1    TO    OUT-CNT
        ELSE
              ADD  1    TO    SKIP-CNT
              GO   TO   NYU-READ-RTN
     END-IF.
 NYU-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***************************

```
