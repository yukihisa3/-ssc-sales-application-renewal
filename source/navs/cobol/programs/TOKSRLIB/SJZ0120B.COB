****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　
*    モジュール名　　　　：　一括在庫引当処理（受注残Ｆ作成）
*    作成日／更新日　　　：　2018/03/13
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　
*    処理概要　　　　　　：　伝票データＦを読込み，商品在庫Ｆ　
*                            と在庫引当を行う。同時に受注残Ｆ
*    　　　　　　　　　　　　も作成する。
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/13 高橋　　　新規作成（ＳＺＡ０１１０Ｂ流用）
*　　　　　　　　（未計上の売上伝票Ｆを読み、在庫引当を行なう）
* 2019/09/18 高橋　　　品単２＝２０の時、１／１０を停止する
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJZ0120B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/29.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  売上伝票ファイル　　    >>---*
     SELECT   SHTDENF  ASSIGN     TO        DA-01-VI-SHTDENL6
                       ORGANIZATION         INDEXED
                       ACCESS     MODE      SEQUENTIAL
                       RECORD     KEY       DEN-F277 DEN-F274
                                            DEN-F09  DEN-F02
                                            DEN-F04  DEN-F051
                                            DEN-F07  DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  受注残ファイル  >>---*
     SELECT   JYUZANF   ASSIGN    TO        DA-01-VI-JYUZANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYU-F03
                                                 JYU-F04
                                                 JYU-F06
                                                 JYU-F07
                                                 JYU-F08
                                                 JYU-F11
                                                 JYU-F05
                        FILE      STATUS    IS   JYU-STATUS.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  売上伝票ファイル　　    >>---*
 FD  SHTDENF.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ>>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  受注残ファイル  >>---*
 FD  JYUZANF.
     COPY     JYUZANF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*売上伝票ファイル退避
     COPY   SHTDENF  OF XFDLIB  JOINING   DE2  AS   PREFIX.
*
 01  SYS-DATE                PIC  9(08).
 01  STATUS-AREA.
     03  DEN-STATUS          PIC  X(02).
     03  OUT-STATUS          PIC  X(02).
     03  ZAI-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  MEI-STATUS          PIC  X(02).
     03  JYU-STATUS          PIC  X(02).
 01  WK-SURYO                PIC  9(10)V99.
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JYUZANF-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  MEI-INV             PIC  9(01)  VALUE ZERO.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  OUT-CNT1            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT2            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT3            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT4            PIC  9(07)  VALUE ZERO.
     03  JYU-CNT             PIC  9(07)  VALUE ZERO.
 01  WRK-AREA.
     03  WRK-TANA            PIC  X(06).
     03  WRK-ZAI             PIC S9(09)V99.
     03  WRK-HIK             PIC S9(09)V99.
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE "SJZ0120B".
       05  FILLER            PIC  X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE " ###".
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SHTDENF.
     MOVE   "SHTDENL6 "      TO   ERR-FL-ID.
     MOVE    DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF "     TO   ERR-FL-ID.
     MOVE      ZAI-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"      TO   ERR-FL-ID.
     MOVE      SHO-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS "      TO   ERR-FL-ID.
     MOVE      MEI-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JYUZANF.
     MOVE     "JYUZANL1"     TO   ERR-FL-ID.
     MOVE      JYU-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
 ZDA0020B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 ZDA0020B-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*ファイルＯＰＥＮ
     OPEN     INPUT     HSHOTBL  HMEIMS.
     OPEN     I-O       SHTDENF  ZAMZAIF    JYUZANF.
*初期化
     MOVE     SPACE          TO   END-FLG.
     MOVE     SPACE          TO   KEP-FLG.
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT1.
     MOVE     ZERO           TO   OUT-CNT2.
*システム日付・時刻の取得
     MOVE     "3"            TO   LINK-IN-KBN.
     ACCEPT   LINK-IN-YMD6   FROM DATE.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD    TO   SYS-DATE.
     ACCEPT   SYS-TIME2      FROM TIME.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      ALL    売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC       SECTION.
*
     READ    SHTDENF
          AT END
             MOVE  "END"       TO   END-FLG
             GO                TO   SHTDENF-READ-EXIT
          NOT AT END
             ADD    1          TO   IN-CNT
     END-READ.
*件数表示
     IF    IN-CNT(5:3) =     "500"  OR  "000"
           DISPLAY  "READ-CNT = " IN-CNT   UPON CONS
     END-IF.
*数量変換
     MOVE  DEN-F15      TO    WK-SURYO.
*#2019/09/18 NAV ST 品単２＝２０の時、数量を1/10しない
*****IF    DEN-F1412    =     "     20 "
*****      COMPUTE           WK-SURYO  =  DEN-F15 / 10
*****END-IF.
*#2019/09/18 NAV ED 品単２＝２０の時、数量を1/10しない
*行が１２より大きい時読み飛ばし
     IF    DEN-F03      >     12
           ADD   1           TO   OUT-CNT4
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*計上済データは対象外とする
     IF    DEN-F277           =    9
           MOVE   "END"      TO   END-FLG
           GO                TO   SHTDENF-READ-EXIT
     END-IF.
*引落済の時　処理対象外
     IF    DEN-F27C     NOT =      ZERO
           ADD   1           TO   OUT-CNT4
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*伝票区分が４０の伝票のみ対象とする
     IF      DEN-F051     =   40
             CONTINUE
     ELSE
             ADD   1         TO   OUT-CNT4
             GO              TO   SHTDENF-READ-SEC
     END-IF.
*ワーク売上伝票ファイル初期化
     MOVE    SPACE           TO   DE2-REC.
     INITIALIZE                   DE2-REC.
*売上伝票ファイル退避
     MOVE    DEN-REC         TO   DE2-REC.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*      2.0 　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
*---<  _番取得  >---*
     MOVE     SPACE          TO   KEP-FLG.
     PERFORM   SHO-READ-SEC.
*---<  在庫引当  >---*
     IF  KEP-FLG    =  SPACE
*********商品変換ＴＢＬが存在する場合、在庫引当処理へ
         PERFORM   ZAIHIKI-SEC
     ELSE
*********売上伝票ファイルを更新する
         PERFORM   REWRITE-SEC
         ADD   1         TO   OUT-CNT3
     END-IF.
 MAIN-READ.
*---<  伝票データＦ　ＲＥＡＤ  >---*
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      2.1     _番取得                                        *
****************************************************************
 SHO-READ-SEC           SECTION.
*
     MOVE    DEN-F01          TO   SHO-F01.
     MOVE    DEN-F25          TO   SHO-F02.
     READ    HSHOTBL
       INVALID      KEY
**********存在しない場合は、欠品ＦＬＧに”１”セット
          MOVE      "1"      TO   KEP-FLG
       NOT INVALID  KEY
**********存在した場合は、_番をＷＫに退避
          MOVE      SHO-F08  TO   WRK-TANA
     END-READ.
*
 SHO-READ-END.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIHIKI-SEC            SECTION.
*在庫マスタ索引
     MOVE    DEN-F08          TO   ZAI-F01.
     MOVE    DEN-F1411        TO   ZAI-F021.
     MOVE    DEN-F1412        TO   ZAI-F022.
     MOVE    WRK-TANA        TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID     MOVE "INV"  TO  ZAMZAIF-INV-FLG
             NOT INVALID MOVE SPACE  TO  ZAMZAIF-INV-FLG
     END-READ.
*
     IF  ZAMZAIF-INV-FLG  =  "INV"
**********在庫マスタが存在しない場合、在庫レコード作成
**********欠品ＦＬＧをセット
          MOVE      "1"      TO   KEP-FLG
*        *在庫マスタ初期化
          MOVE      SPACE    TO   ZAI-REC
          INITIALIZE              ZAI-REC
*        *倉庫ＣＤ
          MOVE    DEN-F08    TO   ZAI-F01
*        *商品ＣＤ／品単ＣＤ
          MOVE    DEN-F1411  TO   ZAI-F021
          MOVE    DEN-F1412  TO   ZAI-F022
*        *_番
          MOVE    WRK-TANA   TO   ZAI-F03
*        *未出庫数加算
          COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO
*        *登録日（システム日付セット）
          MOVE    SYS-DATE   TO   ZAI-F98
*        *名称カナ取得の為、商品名称マスタ読込
          MOVE     ZERO      TO   MEI-INV
          PERFORM  MEI-READ-SEC
*        *商品名称マスタが未存在の場合、在庫更新しない
          IF       MEI-INV   =    ZERO
                   WRITE  ZAI-REC
                   END-WRITE
          END-IF
*        *売上伝票ファイル更新
          PERFORM   REWRITE-SEC
          ADD   1         TO   OUT-CNT2
*        *受注残ファイル作成
*********PERFORM JYUZANF-WT-SEC
     ELSE
**********在庫マスタが存在する場合、在庫レコード更新
*        *現在庫数－引当済数＝引当可能在庫数
          COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28
*        *引当可能在庫数－数量＝引当結果
          COMPUTE   WRK-HIK   =   WRK-ZAI  -  WK-SURYO
*        *引当結果判定
          IF  WRK-HIK  <  0
*            *引当結果が０以下の場合
              MOVE      "1"      TO   KEP-FLG
*            *売上伝票ファイル更新
              PERFORM   REWRITE-SEC
*            *在庫マスタ更新
              PERFORM   UPDATE2-SEC
              ADD   1         TO   OUT-CNT2
          ELSE
*            *売上伝票ファイル更新
              PERFORM   REWRITE-SEC
*            *在庫マスタ更新
              PERFORM   UPDATE-SEC
              ADD   1         TO   OUT-CNT1
          END-IF
     END-IF.
*受注残ファイル作成
     PERFORM JYUZANF-WT-SEC.
*
 ZAIHIKI-END.
     EXIT.
****************************************************************
*      2.2.1   伝票データＦ出力                                *
****************************************************************
 REWRITE-SEC            SECTION.
*
     IF      KEP-FLG   =  SPACE
             MOVE    1         TO     DEN-F27D
     END-IF.
     REWRITE   DEN-REC
     END-REWRITE.
*受注残ファイル更新
*
 REWRITE-END.
     EXIT.
*----------------------------------------------------------*
*                商品名の取得                              *
*----------------------------------------------------------*
 MEI-READ-SEC           SECTION.
*
     MOVE    DEN-F1411        TO   MEI-F01.
     MOVE    DEN-F1412        TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     SPACE     TO   ZAI-F30
          MOVE      1        TO   MEI-INV
       NOT INVALID  KEY
          MOVE     MEI-F031  TO   ZAI-F30
          MOVE     ZERO      TO   MEI-INV
     END-READ.
*
 MEI-READ-END.
     EXIT.
****************************************************************
*      2.2.2   商品在庫マスタ更新１                            *
****************************************************************
 UPDATE-SEC             SECTION.
*
     COMPUTE  ZAI-F28   =   ZAI-F28  +  WK-SURYO.
     COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO.
     REWRITE  ZAI-REC
     END-REWRITE.
*
 UPDATE-END.
     EXIT.
****************************************************************
*              商品在庫マスタ更新２                            *
****************************************************************
 UPDATE2-SEC            SECTION.
*
     COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO.
     REWRITE  ZAI-REC
     END-REWRITE.
*
 UPDATE2-EXIT.
     EXIT.
****************************************************************
*              受注残ファイル作成
****************************************************************
 JYUZANF-WT-SEC         SECTION.
*受注残ファイル初期化
     MOVE     SPACE     TO    JYU-REC.
     INITIALIZE               JYU-REC.
*存在チェック
     MOVE     DE2-F01   TO    JYU-F03.  *>取引先CD
     MOVE     DE2-F02   TO    JYU-F04.  *>伝票番号
     MOVE     DE2-F04   TO    JYU-F06.  *>相殺区分
     MOVE     DE2-F051  TO    JYU-F07.  *>伝票区分
     MOVE     DE2-F07   TO    JYU-F08.  *>店舗CD
     MOVE     DE2-F112  TO    JYU-F11.  *>納品日
     MOVE     DE2-F03   TO    JYU-F05.  *>行番号
     MOVE     DE2-F08   TO    JYU-F09.  *>出荷場所
     MOVE     DE2-F111  TO    JYU-F10.  *>発注日
     MOVE     DE2-F113  TO    JYU-F12.  *>出荷日
     MOVE     DE2-F1411 TO    JYU-F13.  *>サカタ商品CD
     MOVE     DE2-F1412(1:5) TO    JYU-F14.  *>サカタ品単CD1
     MOVE     DE2-F1412(6:2) TO    JYU-F15.  *>サカタ品単CD2
     MOVE     DE2-F1412(8:1) TO    JYU-F16.  *>サカタ品単CD3
     MOVE     WRK-TANA  TO    JYU-F17.  *>_番
     MOVE     DE2-F50   TO    JYU-F18.  *>発注数量
     MOVE     DE2-F15   TO    JYU-F19.  *>出荷数量
     MOVE     DE2-F172  TO    JYU-F20.  *>原価単価
     MOVE     DE2-F173  TO    JYU-F21.  *>売価単価
     COMPUTE  JYU-F22 = DE2-F172 * DE2-F15.  *>原価金額
     COMPUTE  JYU-F23 = DE2-F173 * DE2-F15.  *>売価金額
     MOVE     DE2-F25   TO    JYU-F24.  *>相手商品CD
     IF   KEP-FLG  =  "1"
           MOVE "1"     TO    JYU-F25  *>在庫引当区分
     ELSE
           MOVE SPACE   TO    JYU-F25  *>在庫引当区分
     END-IF.
     IF   DE2-F461 NOT =  ZERO
          MOVE  "1"     TO    JYU-F26  *>オンライン区分
          MOVE  DE2-F46 TO    JYU-F01  *>受信日
          MOVE  DE2-F47 TO    JYU-F02  *>受信時刻
     ELSE
          MOVE  SPACE   TO    JYU-F26  *>オンライン区分
          MOVE  ZERO    TO    JYU-F01  *>受信日
          MOVE  ZERO    TO    JYU-F02  *>受信時刻
**********#2018/05/14 NAV ST　手書、発注数と出荷数は一緒にする
          MOVE  DE2-F15 TO    JYU-F18  *>発注数量
          MOVE  DE2-F15 TO    JYU-F19  *>出荷数量
**********#2018/05/14 NAV ED
     END-IF.
     MOVE     SYS-DATE  TO    JYU-F94.  *>登録日
     MOVE     SYS-TIMEW TO    JYU-F95.  *>登録時刻
     MOVE     SYS-DATE  TO    JYU-F96.  *>更新日
     MOVE     SYS-TIMEW TO    JYU-F97.  *>更新時刻
     MOVE     "2920"    TO    JYU-F98.  *>更新担当者部門
     MOVE     "99"      TO    JYU-F99.  *>更新担当者
*受注残ファイル存在チェック
     READ  JYUZANF
           INVALID      MOVE "INV" TO JYUZANF-INV-FLG
           NOT  INVALID MOVE SPACE TO JYUZANF-INV-FLG
     END-READ.
*
     IF  JYUZANF-INV-FLG  =  "INV"
         WRITE  JYU-REC
         ADD    1        TO   JYU-CNT
     END-IF.
*
 JYUZANF-WT-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     DISPLAY "* SHTDENL6   (IN)= "  IN-CNT   " *" UPON CONS.
     DISPLAY "* ZAMZAIF  (UPD)1= "  OUT-CNT1 " *" UPON CONS.
     DISPLAY "* ZAMZAIF  (UPD)2= "  OUT-CNT2 " *" UPON CONS.
     DISPLAY "* HSHOTBL  (INV) = "  OUT-CNT3 " *" UPON CONS.
     DISPLAY "* TAISYOUGAI     = "  OUT-CNT4 " *" UPON CONS.
     DISPLAY "* JYUZANF    (WT)= "  JYU-CNT  " *" UPON CONS.
*ファイルＣＬＯＳＥ
     CLOSE    SHTDENF    HMEIMS
              ZAMZAIF    HSHOTBL  JYUZANF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
