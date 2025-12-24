****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　ＣＳＶデータ作成                  *
*    作成日／更新日　　　：　03/07/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各編集Ｆを読み、ＣＳＶデータを作成*
*                          する。　　　　　　　　　　　　　　　*
* 2006/10/13 店舗テーブルを２５→３０に変更　　　　　　　　　　*
* 2006/10/13 店舗２０７以上に対応　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0045B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< ＣＳＶ店舗ワーク >>************************************
     SELECT   HACTEN  ASSIGN    TO        DA-01-S-HACTEN
                        FILE      STATUS    IS   TEN-STATUS.
****<< ＣＳＶ商品情報データ  >>*******************************
     SELECT   CSYOSF  ASSIGN    TO        DA-01-S-CSYOSF
                        FILE      STATUS    IS   SYO-STATUS.
****<< ＣＳＶデータ     >>************************************
     SELECT   CDATA   ASSIGN    TO        DA-01-S-CDATA
                        FILE      STATUS    IS   CSV-STATUS.
****<< 倉庫マスタ  >>*****************************************
     SELECT      ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< ＣＳＶ店舗ワーク >>*********************************
 FD  HACTEN
              BLOCK CONTAINS  8   RECORDS.
     COPY     HACTEN OF         XFDLIB
              JOINING  TEN        PREFIX.
****<< ＣＳＶ商品情報ワーク  >>****************************
 FD  CSYOSF
              BLOCK CONTAINS  12  RECORDS.
     COPY     CSYOSF OF         XFDLIB
              JOINING  SYO        PREFIX.
****<< ＣＳＶ商品情報ワーク  >>****************************
 FD  CDATA
              BLOCK CONTAINS  6   RECORDS.
 01  CSV-REC.
     03  CSV-F01             PIC  X(600).
****<< 倉庫マスタ  >>**************************************
 FD  ZSOKMS.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TEN-STATUS           PIC  X(02).
     02 SYO-STATUS           PIC  X(02).
     02 CSV-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0045B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  IZ                      PIC  9(04)  VALUE  ZERO.
 01  IA                      PIC  9(01)  VALUE  ZERO.
 01  BAIKA-KEI               PIC  9(09)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  SYO-CNT             PIC  9(07)  VALUE  ZERO.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  MEISAI-CNT          PIC  9(01)  VALUE  ZERO.
     03  OUTPUT-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEISAI-CNT-P        PIC  9(05)  VALUE  ZERO.
****  キー退避                ****
 01  KEY-BACKUP.
     03  WK-SYO-F02          PIC  X(02)   VALUE  SPACE.
     03  WK-SYO-F03          PIC  9(08)   VALUE  ZERO.
     03  WK-SYO-F06          PIC  9(01)   VALUE  ZERO.
     03  WK-SYO-F07          PIC  9(04)   VALUE  ZERO.
     03  WK-SYO-F15          PIC  9(02)   VALUE  ZERO.
****  日付編集
 01  HENSYU-YMD.
     03  HIDUKE-Y            PIC  9(04)   VALUE  ZERO.
     03  FILLER              PIC  X(01)   VALUE  "/".
     03  HIDUKE-M            PIC  9(02)   VALUE  ZERO.
     03  FILLER              PIC  X(01)   VALUE  "/".
     03  HIDUKE-D            PIC  9(02)   VALUE  ZERO.
****  カウント                ****
 01  TENPO-SAV.
     03  GYO-TBL             OCCURS  14.
         05  TENPO-TBL       OCCURS  30.
             07  TENPO-CD    PIC  9(05).
             07  TENPO-NM    PIC  N(05).
 01  SURYO-SAV.
     03  GYO-TBL             OCCURS  6.
         05  SURYO-GOKEI     PIC  9(05).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*ﾍｯﾀﾞｰﾃﾞｰﾀ
     COPY        CDATA1  OF        XFDLIB
     JOINING     DT1       AS        PREFIX.
*表題1ﾃﾞｰﾀ
     COPY        CDATA2  OF        XFDLIB
     JOINING     DT2       AS        PREFIX.
*表題2ﾃﾞｰﾀ
     COPY        CDATA3  OF        XFDLIB
     JOINING     DT3       AS        PREFIX.
*明細ﾃﾞｰﾀ
     COPY        CDATA4  OF        XFDLIB
     JOINING     DT4       AS        PREFIX.
*ﾌｯﾀｰﾃﾞｰﾀ
     COPY        CDATA5  OF        XFDLIB
     JOINING     DT5       AS        PREFIX.
**
 LINKAGE                SECTION.
 01  LINK-HIDUKE        PIC 9(08).
 01  LINK-JIKAN         PIC 9(04).
************************************************************
 PROCEDURE              DIVISION USING LINK-HIDUKE LINK-JIKAN.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACTEN.
     MOVE   "HACTEN"        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSYOSF.
     MOVE   "CSYOSF"        TO    ERR-FL-ID.
     MOVE    SYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CDATA.
     MOVE   "CDATA "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     HACTEN CSYOSF ZSOKMS.
     OPEN         OUTPUT    CDATA.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    店舗テーブル作成
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14
             PERFORM HACTEN-READ-SEC
             IF      TENPO-END = "END"
                     DISPLAY NC"＃店舗テーブル異常＃" UPON CONS
                     STOP  RUN
             END-IF
             PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 30
                     MOVE  TEN-F021(IY)  TO  TENPO-CD(IX IY)
                     MOVE  TEN-F022(IY)  TO  TENPO-NM(IX IY)
             END-PERFORM
     END-PERFORM.
*
     PERFORM CSYOSF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*    頁番号がﾌﾞﾚｲｸの場合
     IF       SYO-F07 NOT = WK-SYO-F07
*             頁ﾜｰｸが1以上の場合
              IF  PAGE-CNT > ZERO
******************DISPLAY "MEISAI-CNT = " MEISAI-CNT UPON CONS
                IF  MEISAI-CNT < 6
*                   ﾀﾞﾐｰ明細出力
                    ADD 1      TO  MEISAI-CNT
                    PERFORM VARYING IX FROM MEISAI-CNT BY 1
                                            UNTIL IX > 6
********************DISPLAY "IX = " IX UPON CONS
                            PERFORM DUMMY-MEIREC-SEC
                    END-PERFORM
                END-IF
*                 ﾌｯﾀｰ出力
                  PERFORM GOKEI-WRT-SEC
              END-IF
              PERFORM HEAD-WRT1-SEC
              ADD 1          TO  PAGE-CNT
              MOVE SYO-F07   TO  WK-SYO-F07
              MOVE ZERO      TO  WK-SYO-F06  MEISAI-CNT-P
                                 WK-SYO-F15
              MOVE 6         TO  MEISAI-CNT
     END-IF.
*    出力順がﾌﾞﾚｲｸの場合
     IF       SYO-F06 NOT = WK-SYO-F06
*             明細出力ｶｳﾝﾀｰが6以下の場合
              IF  MEISAI-CNT < 6
*                 ﾀﾞﾐｰ明細出力
                  ADD 1      TO  MEISAI-CNT
                  PERFORM VARYING IX FROM MEISAI-CNT BY 1
                                          UNTIL IX > 6
                          PERFORM DUMMY-MEIREC-SEC
                  END-PERFORM
              END-IF
              MOVE  SYO-F06  TO  WK-SYO-F06
              MOVE  SYO-F15  TO  WK-SYO-F15
*             ﾍｯﾀﾞｰ2出力
              ADD   1        TO  MEISAI-CNT-P
              PERFORM  HEAD-WRT2-SEC
              MOVE  ZERO     TO  MEISAI-CNT
     END-IF.
*    明細出力ｶｳﾝﾀｰｶｳﾝﾄｱｯﾌﾟ
     ADD      1              TO  MEISAI-CNT.
     PERFORM  MEISAI-WRT-SEC.
*
     PERFORM CSYOSF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    発注集計表ファイルデータ読込み
************************************************************
 CSYOSF-READ-SEC                 SECTION.
*
     READ    CSYOSF
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        CSYOSF-READ-EXIT
     END-READ.
     ADD     1         TO        CSV-CNT.
*
     IF      CSV-CNT(5:3) = "000" OR "500"
             DISPLAY "READ-CNT   = " CSV-CNT UPON CONS
     END-IF.
*
 CSYOSF-READ-EXIT.
     EXIT.
************************************************************
*    ＣＳＶ店舗ワーク
************************************************************
 HACTEN-READ-SEC                 SECTION.
*
     READ    HACTEN
             AT   END
             MOVE      "END"     TO   TENPO-END
             GO        TO        HACTEN-READ-EXIT
     END-READ.
*
 HACTEN-READ-EXIT.
     EXIT.
************************************************************
*    ﾍｯﾀﾞｰ1出力ｻﾌﾞ
************************************************************
 HEAD-WRT1-SEC                     SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   DT1-REC.
     INITIALIZE                       DT1-REC.
*    カンマセット
     MOVE    ","                 TO   DT1-A01
                                      DT1-A04 DT1-A05 DT1-A06
                                      DT1-A07 DT1-A08 DT1-A09
                                      DT1-A10.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT1-F01.
*    日付+時間+倉庫
     MOVE    LINK-HIDUKE         TO   DT1-F02.
     MOVE    LINK-JIKAN          TO   DT1-F03.
     MOVE    SYO-F02             TO   DT1-F04.
*    ﾃﾞｰﾀ区分ｾｯﾄ
     MOVE    "H  "               TO   DT1-F05.
*    頁番号ｾｯﾄ
     MOVE    SYO-F07             TO   DT1-F06.
*    出荷場所名ｾｯﾄ
     MOVE    SYO-F02             TO   SOK-F01.
     READ    ZSOKMS
             INVALID
               MOVE  X"28"       TO   DT1-F071
               MOVE  ALL NC"＊"  TO   DT1-F072
               MOVE  X"29"       TO   DT1-F073
             NOT INVALID
               MOVE  X"28"       TO   DT1-F071
               MOVE  SOK-F02     TO   DT1-F072
               MOVE  X"29"       TO   DT1-F073
     END-READ.
*    企画名
     MOVE      X"28"             TO   DT1-F081.
     MOVE      NC"オンライン分"  TO   DT1-F082.
     MOVE      X"29"             TO   DT1-F083.
*    投入日
     MOVE      SYO-F03(1:4)      TO   HIDUKE-Y.
     MOVE      SYO-F03(5:2)      TO   HIDUKE-M.
     MOVE      SYO-F03(7:2)      TO   HIDUKE-D.
     MOVE      HENSYU-YMD        TO   DT1-F10.
*    出荷名称
     MOVE      X"28"             TO   DT1-F111.
     MOVE      ALL NC"　"        TO   DT1-F112.
     MOVE      X"29"             TO   DT1-F113.
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT1-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*    数量合計初期化
     MOVE      ZERO              TO   SURYO-SAV.
*
 HEAD-WRT1-EXIT.
     EXIT.
************************************************************
*    ﾍｯﾀﾞｰ2出力ｻﾌﾞ
************************************************************
 HEAD-WRT2-SEC                     SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   DT2-REC  DT3-REC.
     INITIALIZE                       DT2-REC  DT3-REC.
*    カンマセット
     MOVE    ","                 TO   DT2-A01
                                      DT2-A04 DT2-A05 DT2-A06
                                      DT2-A07 DT2-A08 DT2-A09
                                      DT2-A10 DT2-A11.
     MOVE    ","                 TO   DT3-A01
                                      DT3-A04 DT3-A05 DT3-A06
                                      DT3-A07 DT3-A08 DT3-A09
                                      DT3-A10 DT3-A11.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT2-F01.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT3-F01.
*    日付+時間+倉庫
     MOVE    LINK-HIDUKE         TO   DT2-F02 DT3-F02.
     MOVE    LINK-JIKAN          TO   DT2-F03 DT3-F03.
     MOVE    SYO-F02             TO   DT2-F04 DT3-F04.
*    ﾃﾞｰﾀ区分ｾｯﾄ
     MOVE    "BT1"               TO   DT2-F05.
     MOVE    "BT2"               TO   DT3-F05.
*    頁番号ｾｯﾄ
     MOVE    SYO-F07             TO   DT2-F06 DT3-F06.
*    ＪＡＮＣＤ名ｾｯﾄ
     MOVE      X"28"             TO   DT2-F071.
     MOVE      NC"ＪＡＮコード"  TO   DT2-F072.
     MOVE      X"29"             TO   DT2-F073.
*    商品名
     MOVE      X"28"             TO   DT2-F081.
     MOVE      NC"商品名"        TO   DT2-F082.
     MOVE      X"29"             TO   DT2-F083.
*    規格
     MOVE      X"28"             TO   DT2-F091.
     MOVE      NC"規格"          TO   DT2-F092.
     MOVE      X"29"             TO   DT2-F093.
*    入数
     MOVE      X"28"             TO   DT2-F101.
     MOVE      NC"入数"          TO   DT2-F102.
     MOVE      X"29"             TO   DT2-F103.
*    売価
     MOVE      X"28"             TO   DT2-F111.
     MOVE      NC"売価"          TO   DT2-F112.
     MOVE      X"29"             TO   DT2-F113.
*    店舗情報ｾｯﾄ
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE TENPO-CD(WK-SYO-F15 IX) TO DT2-F121(IX)
             MOVE ","                     TO DT2-F122(IX)
             MOVE X"28"                   TO DT3-F121(IX)
             MOVE TENPO-NM(WK-SYO-F15 IX) TO DT3-F122(IX)
             MOVE X"29"                   TO DT3-F123(IX)
             MOVE ","                     TO DT3-A12 (IX)
***** DISPLAY "TENPO-CD   = " TENPO-CD(WK-SYO-F06 IX) UPON CONS
***** DISPLAY "WK-SYO-F06 = " WK-SYO-F06              UPON CONS
***** DISPLAY "IX         = " IX                      UPON CONS
     END-PERFORM.
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT2-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT3-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*
 HEAD-WRT2-EXIT.
     EXIT.
************************************************************
*    明細出力ｻﾌﾞ
************************************************************
 MEISAI-WRT-SEC                    SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   DT4-REC.
     INITIALIZE                       DT4-REC.
*    カンマセット
     MOVE    ","                 TO   DT4-A01
                                      DT4-A04 DT4-A05 DT4-A06
                                      DT4-A07 DT4-A08 DT4-A09
                                      DT4-A10 DT4-A11.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT4-F01.
*    日付+時間+倉庫
     MOVE    LINK-HIDUKE         TO   DT4-F02.
     MOVE    LINK-JIKAN          TO   DT4-F03.
     MOVE    SYO-F02             TO   DT4-F04.
*    ﾃﾞｰﾀ区分ｾｯﾄ
     MOVE    "BM "               TO   DT4-F05.
*    頁番号ｾｯﾄ
     MOVE    SYO-F07             TO   DT4-F06.
*    ＪＡＮＣＤ
     MOVE    SYO-F04             TO   DT4-F07.
*    商品名称
     MOVE    SYO-F111            TO   DT4-F08(1:15).
     MOVE    SYO-F112            TO   DT4-F08(16:15).
*    規格
     MOVE    SYO-F08             TO   DT4-F09.
*    入数
     MOVE    SYO-F10             TO   DT4-F10.
*    売価
     MOVE    SYO-F13             TO   DT4-F11.
*    数量
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE  SYO-F14(IX)   TO   DT4-F121(IX)
             MOVE  ","           TO   DT4-F122(IX)
             COMPUTE BAIKA-KEI   = BAIKA-KEI +
                                 ( SYO-F13 * SYO-F14(IX) )
             COMPUTE SURYO-GOKEI(MEISAI-CNT) =
                     SURYO-GOKEI(MEISAI-CNT) + SYO-F14(IX)
     END-PERFORM.
*    合計数量セット
**** IF  MEISAI-CNT-P = 7
****     MOVE SURYO-GOKEI(MEISAI-CNT) TO DT4-F121(24)
**** END-IF.
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT4-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*
 MEISAI-WRT-EXIT.
     EXIT.
************************************************************
*    明細ﾀﾞﾐｰｻﾌﾞ
************************************************************
 DUMMY-MEIREC-SEC                  SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   DT4-REC.
     INITIALIZE                       DT4-REC.
*    カンマセット
     MOVE    ","                 TO   DT4-A01
                                      DT4-A04 DT4-A05 DT4-A06
                                      DT4-A07 DT4-A08 DT4-A09
                                      DT4-A10 DT4-A11.
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT4-F01.
*    日付+時間+倉庫
     MOVE    LINK-HIDUKE         TO   DT4-F02.
     MOVE    LINK-JIKAN          TO   DT4-F03.
     MOVE    SYO-F02             TO   DT4-F04.
*    ﾃﾞｰﾀ区分ｾｯﾄ
     MOVE    "BM "               TO   DT4-F05.
*    頁番号ｾｯﾄ
     MOVE    SYO-F07             TO   DT4-F06.
*    数量
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 30
             MOVE  ","           TO   DT4-F122(IZ)
     END-PERFORM.
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT4-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*
 DUMMY-MEIREC-EXIT.
     EXIT.
************************************************************
*    明細ﾀﾞﾐｰｻﾌﾞ
************************************************************
 GOKEI-WRT-SEC                     SECTION.
*    ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   DT5-REC.
     INITIALIZE                       DT5-REC.
*    カンマセット
     MOVE    ","                 TO   DT5-A01
                                      DT5-A04 DT5-A05 DT5-A06
                                      DT5-A07 DT5-A08 DT5-A09
                                      DT5-A10 .
*    通し番号ｶｳﾝﾄｱｯﾌﾟ
     ADD     1                   TO   OUTPUT-CNT.
     MOVE    OUTPUT-CNT          TO   DT5-F01.
*    日付+時間+倉庫
     MOVE    LINK-HIDUKE         TO   DT5-F02.
     MOVE    LINK-JIKAN          TO   DT5-F03.
     MOVE    SYO-F02             TO   DT5-F04.
*    ﾃﾞｰﾀ区分ｾｯﾄ
     MOVE    "F  "               TO   DT5-F05.
*    頁番号ｾｯﾄ
     MOVE    SYO-F07             TO   DT5-F06.
*    投入日
     MOVE    SPACE               TO   DT5-F07.
*    原価金額合計
     MOVE    ZERO                TO   DT5-F08.
*    原価金額合計
     MOVE    BAIKA-KEI           TO   DT5-F09.
*    作成日
     MOVE    SYS-DATEW(1:4)      TO   HIDUKE-Y.
     MOVE    SYS-DATEW(5:2)      TO   HIDUKE-M.
     MOVE    SYS-DATEW(7:2)      TO   HIDUKE-D.
     MOVE    HENSYU-YMD          TO   DT5-F10.
*    数量
*    出力ﾚｺｰﾄﾞ初期化
     MOVE    SPACE               TO   CSV-REC.
     INITIALIZE                       CSV-REC.
*    ﾚｺｰﾄﾞｾｯﾄ
     MOVE      DT5-REC           TO   CSV-REC.
*    ﾚｺｰﾄﾞ出力
     WRITE     CSV-REC.
*    売価合計初期化
     MOVE      ZERO              TO   BAIKA-KEI.
*
 GOKEI-WRT-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     IF  PAGE-CNT > ZERO
         IF  MEISAI-CNT < 6
*            ﾀﾞﾐｰ明細出力
             ADD 1      TO  MEISAI-CNT
             PERFORM VARYING IX FROM MEISAI-CNT BY 1
                                     UNTIL IX > 6
                     PERFORM DUMMY-MEIREC-SEC
             END-PERFORM
         END-IF
*        ﾌｯﾀｰ出力
         PERFORM GOKEI-WRT-SEC
     END-IF.
*ﾌｧｲﾙCLOSE
     CLOSE             HACTEN CSYOSF ZSOKMS CDATA.
*ﾚｺｰﾄﾞｶｳﾝﾀｰ情報出力
     DISPLAY "READ-CNT   = " CSV-CNT     UPON CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
