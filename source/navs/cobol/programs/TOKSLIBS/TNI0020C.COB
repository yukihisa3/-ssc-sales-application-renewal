****************************************************************
*新基幹システムへ変更します。1999/10/16 T.TAKAHASHI
*    顧客名　　　　　　　：　（株）サカタのタネ　殿　　　　　　
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　
*    モジュール名　　　　：　在庫引落し　　　　　　　　　　　　
*    作成日／更新日　　　：　93/05/06
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　伝票データファイルを読み，抽出条件
*                            を満たすデータを商品在庫マスタと共
*                            に更新する．
*    更新履歴            ：
*      1993/06/15 入力ファイルをＤＥＮＪＮＬ６に変更
*                 計上済データは対象外　　　　　　　
*      1994/05/19 商品在庫マスタ更新の変更　　　　　
*                 （出荷日ベースの更新）　　　　　　
*      2011/10/06 飯田/NAV 基幹サーバ統合
*      2013/10/31 高橋　売上計上区分が計上済でも在庫更新未の場合
*                       は在庫更新処理をする様に変更する。
*      2018/07/23 高橋　ＤＣＭケーヨー３０行返品伝票対応
*      ３０行解除取引先ＣＤ　１７３１、１７３２、７６０１、
*                            ７６０２
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TNI0020C.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  伝票データファイル　    >>---*
     SELECT   HDENJNL  ASSIGN     TO        DA-01-VI-DENJNL6
                       ORGANIZATION        INDEXED
                       ACCESS     MODE      SEQUENTIAL
                       RECORD     KEY       DEN-F277 DEN-F274
                                            DEN-F09  DEN-F02
                                       *> 2011/10/06,S  S.I/NAV
                                            DEN-F04
                                            DEN-F051
                                            DEN-F07
                                            DEN-F112
                                       *> 2011/10/06,E  S.I/NAV
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*
*---<<  商品在庫マスタ　　      >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*
*---<<  商品コード変換テーブル　>>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F04
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SH1-F01
                                                 SH1-F02
                        FILE      STATUS    IS   SH1-STATUS.
*
*---<<  条件ファイル　　　　　　>>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<<  商品名称マスタ　　　　　>>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  伝票データファイル　    >>---*
 FD  HDENJNL.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*---<<  商品在庫マスタ　　      >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品コード変換テーブル　>>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
 FD  SHOTBL1.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SH1       PREFIX.
*---<<  条件ファイル　　　　　　>>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<              　　　　　　>>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  DEN-STATUS          PIC  X(02).
     02  ZAI-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  SH1-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
****  フラグ  ***
 01  FLG-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  INV-FLG             PIC  9(01)  VALUE ZERO.
****  カウンタ ***
 01  READ-CNT                PIC  9(07)  VALUE ZERO.
 01  CNT-AREA.
     02  READ-DEN-CNT        PIC  9(07)  VALUE ZERO.
     02  REWRITE-DEN-CNT     PIC  9(07)  VALUE ZERO.
     02  SYORI-DEN-CNT       PIC  9(07)  VALUE ZERO.
     02  JUMP-DEN-CNT        PIC  9(07)  VALUE ZERO.
     02  REWRITE-ZAI-CNT     PIC  9(07)  VALUE ZERO.
     02  WRITE-ZAI-CNT       PIC  9(07)  VALUE ZERO.
****  システム日付け　****
 01  SYS-AREA.
     02  SYS-Y               PIC  9(02).
     02  SYS-DATE            PIC  9(06).
 01  SYS-AREA-R    REDEFINES    SYS-AREA
                             PIC  9(08).
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-SURYO            PIC S9(09)V99  VALUE  ZERO.
     02  WK-NEN              PIC  9(04)  VALUE  ZERO.
     02  WK-ZAIKO            PIC  9(06)  VALUE  ZERO.
     02  WK-ZAIKOR   REDEFINES    WK-ZAIKO.
       03  WK-ZAIKO1         PIC  9(04).
       03  WK-ZAIKO2         PIC  9(02).
     02  WK-SYUKA            PIC  9(08).
     02  WK-SYUKAR   REDEFINES    WK-SYUKA.
       03  WK-SYUKA1         PIC  9(06).
       03  WK-SYUKA2         PIC  9(02).
 01  WK-TANABAN              PIC  X(06)  VALUE  SPACE.
 01  WK-TOKCD                PIC  9(08)  VALUE  ZERO.
 01  CYU-DATE                PIC  9(08)  VALUE  ZERO.
 01  NOU-DATE                PIC  9(08)  VALUE  ZERO.
 01  SYU-DATE                PIC  9(08)  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "TNI0020C".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 01    X                     PIC  X(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HDENJNL.
     MOVE     "HDENJNL"      TO   ERR-FL-ID.
     MOVE     DEN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF"      TO   ERR-FL-ID.
     MOVE     ZAI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-SEC31               SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SHOTBL1.
     MOVE     "SHOTBL1"      TO   ERR-FL-ID.
     MOVE     SH1-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
****************************************************************
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS "      TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*     １．０　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             HDENJNL
                             ZAMZAIF
             INPUT           HSHOTBL
                             SHOTBL1
                             HJYOKEN
                             HMEIMS.
*
*****　更新日を取得する　****
     MOVE    99              TO   JYO-F01.
     MOVE    "DAY"           TO   JYO-F02.
     READ    HJYOKEN.
     MOVE    JYO-F04         TO   WK-TOKCD.
     MOVE    JYO-F05         TO   CYU-DATE.
     MOVE    JYO-F06         TO   NOU-DATE.
     MOVE    JYO-F07         TO   SYU-DATE.
*
     DISPLAY "対象得意先　＝　"   WK-TOKCD  UPON CONS.
     DISPLAY "対象注文日　＝　"   CYU-DATE  UPON CONS.
     DISPLAY "対象納品日　＝　"   NOU-DATE  UPON CONS.
     DISPLAY "対象出荷日　＝　"   SYU-DATE  UPON CONS.
*
*****　システム日付　取得　****
     ACCEPT    SYS-DATE    FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE      SYS-DATE           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-AREA.
****   最終月次更新日取得  ****
     MOVE    99              TO   JYO-F01.
     MOVE    "ZAI"           TO   JYO-F02.
     READ    HJYOKEN.
     MOVE    JYO-F05         TO   WK-ZAIKO.
     ADD     1               TO   WK-ZAIKO2.
****   最終更新日＋１が１２以上の時，年＋１，月に１を転送する
     IF      WK-ZAIKO2  >  12
             ADD     1       TO   WK-ZAIKO1
             MOVE    1       TO   WK-ZAIKO2
     END-IF.
*****　伝票データＦ　ＲＥＡＤ　****
     PERFORM    HDENJNL-READ-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      １．１　　伝票データＦ　ＲＥＡＤ処理                    *
****************************************************************
 HDENJNL-READ-SEC              SECTION.
*
     READ    HDENJNL
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   HDENJNL-READ-END
     END-READ.
     ADD   1       TO             READ-CNT.
     IF    READ-CNT(5:3)     =    "000"   OR
                                  "500"
           DISPLAY  "READ = " READ-CNT UPON CONS
     END-IF.
 DENREAD-10.
     IF    DEN-F03     >     12
*#2018/07/23 NAV ST ＤＣＭケーヨー対応
*******IF  ( DEN-F01 = 173 OR DEN-F01 = 2243 OR DEN-F01 = 72028 )
       IF  ( DEN-F01 = 173 OR DEN-F01 = 2243 OR DEN-F01 = 72028
       OR    DEN-F01 = 1731 OR DEN-F01 = 1732 OR DEN-F01 = 7601
       OR    DEN-F01 = 7602 )
*#2018/07/23 NAV ED ＤＣＭケーヨー対応
       AND DEN-F03  <= 30
           CONTINUE
       ELSE
           ADD   1       TO   JUMP-DEN-CNT
           GO            TO   HDENJNL-READ-SEC
       END-IF
     END-IF.
*2013/10/31 NAV ST 下記の条件判断を停止
** 売上データ作成区分　９　の時終了
*****IF    DEN-F277          =    9
*****      MOVE   "END"      TO   END-FLG
*****      GO                TO   HDENJNL-READ-END
*****END-IF.
*2013/10/31 NAV ED 下記の条件判断を停止
     ADD   1                 TO   READ-DEN-CNT.
*    引落済の時　処理対象外
     IF    DEN-F27C    NOT =      ZERO
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
*
 DENREAD-60.
*    付番フラグが　９　でないとき対象外
     IF    DEN-F276    NOT =      9
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
*
 DENREAD-61.
*    得意先コードが一致した場合のみ対象
     IF  WK-TOKCD         NOT =   ZERO
         IF DEN-F01       NOT =   WK-TOKCD
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
 DENREAD-62.
*
*    納品日が入力された納品日より大きい時対象外
     IF DEN-F112          >    NOU-DATE
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
 DENREAD-63.
*    出荷日が未入力の時納入日が入力された納入日より　
*　　大きい時対象外
     IF    DEN-F113          =    ZERO     AND
           SYU-DATE          =    ZERO
        IF DEN-F112          >    NOU-DATE
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
        END-IF
     END-IF.
*
 DENREAD-64.
* 出荷日が０の時は納入日（伝票Ｆ）を転送
* 出荷日指定の時は出荷日（条件Ｆ）を転送
     IF      DEN-F113   =    ZERO
             IF    SYU-DATE  =    ZERO
                   MOVE   DEN-F112     TO   DEN-F113
             ELSE
               IF  DEN-F112 >  SYU-DATE
                   MOVE   SYU-DATE     TO   DEN-F113
               ELSE
                   MOVE   DEN-F112     TO   DEN-F113
               END-IF
             END-IF
     END-IF.

 DENREAD-65.
*    出荷日がシステム日付より大きい時対象外
     IF    DEN-F113     >  SYS-AREA-R
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
*
 DENREAD-70.
*    伝票区分が４０または４１の伝票のみ対象とする
     IF      DEN-F051    =   40   OR  41
         ADD   1             TO   SYORI-DEN-CNT
     ELSE
           ADD   1           TO   JUMP-DEN-CNT
           GO                TO   HDENJNL-READ-SEC
     END-IF.
*
 HDENJNL-READ-END.
     EXIT.
****************************************************************
*      ２．０　　メイン処理                                    *
****************************************************************
 MAIN-SEC                    SECTION.
***　_番取得　処理　　　　      ****
     PERFORM    TANABAN-GET-SEC.
***  商品変換テーブルが存在しない時は、更新しない。
*****IF         INV-FLG      =    ZERO
***  2000/06/16 更新処理（更新処理は_番がなくても更新する事）
                PERFORM    KOUSHIN-SEC.
*****END-IF.
***  伝票データＦＲＥＡＤ　処理　****
     PERFORM    HDENJNL-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      ２．１　　_番取得　処理                                *
****************************************************************
 TANABAN-GET-SEC             SECTION.
     MOVE   ZERO             TO   INV-FLG.
****　商品コード変換テーブル　検索　****
     IF      DEN-F25   NOT   =    SPACE
*
             MOVE    DEN-F01         TO   SH1-F01
             MOVE    DEN-F25         TO   SH1-F02
             READ    SHOTBL1
                  INVALID   KEY
                     MOVE    1       TO   INV-FLG
                  NOT INVALID    KEY
                     MOVE    SH1-F08 TO   WK-TANABAN
                     GO              TO   TANABAN-GET-END
             END-READ
     END-IF.
*
 TANABAN-GET-50.
*
     MOVE    DEN-F01         TO   SHO-F01.
     MOVE    DEN-F08         TO   SHO-F04
     MOVE    DEN-F1411       TO   SHO-F031
     MOVE    DEN-F1412       TO   SHO-F032
     READ    HSHOTBL
           INVALID   KEY
             MOVE    1       TO   INV-FLG
           NOT INVALID    KEY
             MOVE    SHO-F08 TO   WK-TANABAN
             MOVE    ZERO    TO   INV-FLG
     END-READ.
*
 TANABAN-GET-END.
     EXIT.
****************************************************************
*      ２．２　　更新　処理　　                                *
****************************************************************
 KOUSHIN-SEC                 SECTION.
*
* 出荷日が０の時は納入日（伝票Ｆ）を転送
* 出荷日指定の時は出荷日（条件Ｆ）を転送
     IF      DEN-F113   =    ZERO
             IF    SYU-DATE  =    ZERO
                   MOVE   DEN-F112     TO   DEN-F113
             ELSE
               IF  DEN-F112 >  SYU-DATE
                   MOVE   SYU-DATE     TO   DEN-F113
               ELSE
                   MOVE   DEN-F112     TO   DEN-F113
               END-IF
             END-IF
     END-IF.
*
     MOVE    DEN-F113      TO              WK-SYUKA.
*
****　商品在庫マスタ　検索　****
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     IF      INV-FLG         =    1
             MOVE    SPACE   TO   ZAI-F03
     ELSE
             MOVE    WK-TANABAN   TO  ZAI-F03
     END-IF.
     READ    ZAMZAIF
         INVALID   KEY
                   GO        TO   KOUSHIN-010
     END-READ.
*
****　商品在庫マスタ更新　更新　****
*------  伝票区分＝４０の時　------*
     IF      DEN-F051   =    40
             PERFORM    EDIT-040-SEC
     END-IF.
*------  伝票区分＝４１の時　------*
     IF      DEN-F051   =    41
             PERFORM    EDIT-041-SEC
     END-IF.
     MOVE       SYS-AREA-R   TO   ZAI-F99.
     REWRITE    ZAI-REC.
     ADD   1                 TO   REWRITE-ZAI-CNT.
     GO            TO   KOUSHIN-020.
*
 KOUSHIN-010.
****　商品在庫マスタ追加　処理　****
     MOVE   SPACE            TO   ZAI-REC.
     INITIALIZE                   ZAI-REC.
     MOVE   DEN-F08          TO   ZAI-F01.
     MOVE   DEN-F1411        TO   ZAI-F021.
     MOVE   DEN-F1412        TO   ZAI-F022.
     IF      INV-FLG         =    1
             MOVE    SPACE   TO   ZAI-F03
     ELSE
             MOVE WK-TANABAN TO   ZAI-F03
     END-IF.
     MOVE    DEN-F1411       TO   MEI-F01.
     MOVE    DEN-F1412       TO   MEI-F012.
     READ    HMEIMS
         INVALID
*            商品名称マスタに存在しない時は、更新しない。
             GO   TO    KOUSHIN-END
*************MOVE SPACE      TO   ZAI-F30
         NOT INVALID
             MOVE MEI-F031   TO   ZAI-F30
     END-READ.
*------  伝票区分＝４０の時　------*
     IF      DEN-F051   =    40
             PERFORM    EDIT-040-SEC
     END-IF.
*------  伝票区分＝４１の時　------*
     IF      DEN-F051   =    41
             PERFORM    EDIT-041-SEC
     END-IF.
     MOVE     SYS-AREA-R   TO   ZAI-F98.
     MOVE     SYS-AREA-R   TO   ZAI-F99.
     WRITE    ZAI-REC.
     ADD   1                 TO   WRITE-ZAI-CNT.
*
 KOUSHIN-020.
****　伝票データＦの更新　処理　****
* 在庫引落フラグ
     MOVE    1               TO   DEN-F27C.
     REWRITE    DEN-REC.
     ADD   1                 TO   REWRITE-DEN-CNT.
*
 KOUSHIN-END.
     EXIT.
****************************************************************
*      ２．２．１　編集　処理４０                              *
****************************************************************
 EDIT-040-SEC                SECTION.
****　数量を１０分の１にするか，そのまま使用するか決定する　****
     IF   DEN-F1412   =   "     20 "
         COMPUTE    WK-SURYO   ROUNDED  =   DEN-F15   /   10
     ELSE
         MOVE    DEN-F15     TO   WK-SURYO
     END-IF.
****　相殺区分（伝票データ）＝０の時　****
     IF  DEN-F04    =   ZERO
         COMPUTE    ZAI-F27   =   ZAI-F27   -   WK-SURYO
         IF         ZAI-F27   <   ZERO
                    MOVE          ZERO      TO  ZAI-F27
         END-IF
         COMPUTE    ZAI-F04   =   ZAI-F04   -   WK-SURYO
         IF   WK-ZAIKO       <    WK-SYUKA1
              COMPUTE   ZAI-F12   =    ZAI-F12   +    WK-SURYO
***           COMPUTE   ZAI-FII   =    ZAI-FII   -    WK-SURYO
                    COMPUTE    ZAI-F13  =
                               ZAI-F13     +   WK-SURYO
         ELSE
              COMPUTE   ZAI-F08   =    ZAI-F08   +    WK-SURYO
              COMPUTE   ZAI-F06   =    ZAI-F06   -    WK-SURYO
                    COMPUTE    ZAI-F09  =
                               ZAI-F09     +   WK-SURYO
         END-IF
*
         IF   DEN-F27D       =   1
              COMPUTE   ZAI-F28   =    ZAI-F28   -    WK-SURYO
              IF        ZAI-F28   <    ZERO
                        MOVE           ZERO      TO   ZAI-F28
              END-IF
         END-IF
     END-IF.
*
****　相殺区分（伝票データ）＝奇数の時　****
     IF   DEN-F04   =   1 OR 3 OR 5 OR 7 OR 9
         COMPUTE    ZAI-F04   =   ZAI-F04   +   WK-SURYO
         IF   WK-ZAIKO       <    WK-SYUKA1
              COMPUTE   ZAI-F12   =    ZAI-F12   -    WK-SURYO
***           COMPUTE   ZAI-FII   =    ZAI-FII   +    WK-SURYO
                    COMPUTE    ZAI-F13  =
                               ZAI-F13  -   WK-SURYO
         ELSE
              COMPUTE   ZAI-F08   =    ZAI-F08   -    WK-SURYO
              COMPUTE   ZAI-F06   =    ZAI-F06   +    WK-SURYO
                    COMPUTE    ZAI-F09  =
                               ZAI-F09  -   WK-SURYO
         END-IF
     END-IF.
****　相殺区分（伝票データ）＝偶数の時　****
     IF   DEN-F04   =   2 OR 4 OR 6 OR 8
         COMPUTE    ZAI-F04   =   ZAI-F04   -   WK-SURYO
         IF   WK-ZAIKO       <    WK-SYUKA1
              COMPUTE   ZAI-F12   =    ZAI-F12   +    WK-SURYO
***           COMPUTE   ZAI-FII   =    ZAI-FII   -    WK-SURYO
                    COMPUTE    ZAI-F13  =
                               ZAI-F13  +   WK-SURYO
         ELSE
              COMPUTE   ZAI-F08   =    ZAI-F08   +    WK-SURYO
              COMPUTE   ZAI-F06   =    ZAI-F06   -    WK-SURYO
                    COMPUTE    ZAI-F09  =
                               ZAI-F09  +   WK-SURYO
         END-IF
     END-IF.
*
 EDIT-040-END.
     EXIT.
****************************************************************
*      ２．２．２　編集　処理４１                              *
****************************************************************
 EDIT-041-SEC                SECTION.
****　数量を１０分の１にするか，そのまま使用するか決定する　****
     IF   DEN-F1412   =   "     20 "
         COMPUTE    WK-SURYO   ROUNDED   =   DEN-F15   /   10
     ELSE
         MOVE    DEN-F15     TO   WK-SURYO
     END-IF.
****　相殺区分（伝票データ）＝偶数の時　****
     IF   DEN-F04   =   ZERO   OR   2 OR 4 OR 6 OR 8
         COMPUTE    ZAI-F04   =   ZAI-F04   +   WK-SURYO
         IF   WK-ZAIKO       <    WK-SYUKA1
              COMPUTE   ZAI-F12   =    ZAI-F12   -    WK-SURYO
***           COMPUTE   ZAI-FII   =    ZAI-FII   +    WK-SURYO
              COMPUTE   ZAI-F13   =    ZAI-F13   -    WK-SURYO
*             次月返品数
              COMPUTE   ZAI-F14   =    ZAI-F14   +    WK-SURYO
         ELSE
              COMPUTE   ZAI-F08   =    ZAI-F08   -    WK-SURYO
              COMPUTE   ZAI-F06   =    ZAI-F06   +    WK-SURYO
              COMPUTE   ZAI-F09   =    ZAI-F09   -    WK-SURYO
*             当月返品数
              COMPUTE   ZAI-F10   =    ZAI-F10   +    WK-SURYO
         END-IF
     END-IF.
****　相殺区分（伝票データ）＝奇数の時　****
     IF   DEN-F04   =   1      OR   3 OR 5 OR 7 OR 9
         COMPUTE    ZAI-F04   =   ZAI-F04   -   WK-SURYO
         IF   WK-ZAIKO       <    WK-SYUKA1
              COMPUTE   ZAI-F12   =    ZAI-F12   +    WK-SURYO
***           COMPUTE   ZAI-FII   =    ZAI-FII   -    WK-SURYO
              COMPUTE   ZAI-F13   =    ZAI-F13   +    WK-SURYO
*             次月返品数
              COMPUTE   ZAI-F14   =    ZAI-F14   -    WK-SURYO
         ELSE
              COMPUTE   ZAI-F08   =    ZAI-F08   +    WK-SURYO
              COMPUTE   ZAI-F06   =    ZAI-F06   -    WK-SURYO
              COMPUTE   ZAI-F09   =    ZAI-F09   +    WK-SURYO
*             当月返品数
              COMPUTE   ZAI-F10   =    ZAI-F10   -    WK-SURYO
         END-IF
     END-IF.
 EDIT-041-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              HDENJNL
                        ZAMZAIF
                        HSHOTBL
                        SHOTBL1
                        HJYOKEN
                        HMEIMS.
     DISPLAY "ﾃﾞﾝﾋﾟﾖｳﾃﾞ-ﾀ(ﾆﾕｳﾘﾖｸ) = " READ-DEN-CNT    UPON CONS.
     DISPLAY "ﾃﾞﾝﾋﾟﾖｳﾃﾞ-ﾀ  (ｺｳｼﾝ) = " REWRITE-DEN-CNT UPON CONS.
     DISPLAY "ﾃﾞﾝﾋﾟﾖｳﾃﾞ-ﾀ   (ｼﾖﾘ) = " SYORI-DEN-CNT   UPON CONS.
     DISPLAY "ﾃﾞﾝﾋﾟﾖｳﾃﾞ-ﾀ(ﾖﾐﾄﾊﾞｼ) = " JUMP-DEN-CNT    UPON CONS.
     DISPLAY "ｼﾖｳﾋﾝｻﾞｲｺM   (ｺｳｼﾝ) = " REWRITE-ZAI-CNT UPON CONS.
     DISPLAY "ｼﾖｳﾋﾝｻﾞｲｺM   (ﾂｲｶ ) = " WRITE-ZAI-CNT   UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
