# NVD0530L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0530L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　倉庫間在庫移動実績リスト　　　　　*
*    作成日／作成者　　　：　2020/04/24   ASS.II               *
*    処理内容　　　　　　：　新入出庫ファイルより倉庫間在庫　　*
*    　　　　　　　　　　　　移動実績リストを出力する。　　　　*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         NVD0530L.
 AUTHOR.             ASS.II.
 DATE-WRITTEN.       02.04.24.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YA-22     IS   YA-22
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*新入出庫Ｆ
     SELECT   DNSFILF4  ASSIGN        TO  DA-01-VI-DNSFILL4
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS4-F05 DNS4-F12
                                          DNS4-F01 DNS4-F02
                        FILE STATUS   IS  DNS4-STA.
     SELECT   DNSFILF5  ASSIGN        TO  DA-01-VI-DNSFILL5
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS5-F08 DNS5-F18
                                          DNS5-F01 DNS5-F02
                        FILE STATUS   IS  DNS5-STA.
     SELECT   DNSFILF7  ASSIGN        TO  DA-01-VI-DNSFILL7
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS7-F05 DNS7-F14
                                          DNS7-F01 DNS7-F02
                        FILE STATUS   IS  DNS7-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1   ASSIGN        TO  DA-01-VI-ZSOKMS1
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  RANDOM
                        RECORD KEY    IS  SOK-F01
                        FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
     SELECT   MEIMS1    ASSIGN        TO  DA-01-VI-MEIMS1
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  RANDOM
                        RECORD    KEY     MEI-F011   MEI-F0121
                                          MEI-F0122  MEI-F0123
                        FILE STATUS   IS  MEI-STA.

*プリンタＦ
     SELECT   PRINTF    ASSIGN  TO   LP-04.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*新入出庫ファイル
 FD  DNSFILF4           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS4  AS PREFIX.
 FD  DNSFILF5           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS5  AS PREFIX.
 FD  DNSFILF7           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS7  AS PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1            LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*商品名称Ｍ
 FD  MEIMS1             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*プリンタＦ
 FD  PRINTF.
 01    P-REC                 PIC  X(200).
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  DNS4-STA            PIC  X(02).
     03  DNS5-STA            PIC  X(02).
     03  DNS7-STA            PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  MEI-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER              PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN          PIC  N(06)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"）".
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SOKCD            PIC  X(02)  VALUE  SPACE.
     03  WK-TANCD            PIC  X(02)  VALUE  SPACE.
     03  MAX-LINE            PIC  9(03)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
     03  IX1                 PIC  9(03)  VALUE  ZERO.
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*新入出庫ファイル読込エリア
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS  AS PREFIX.
****  見出し行０             ****
 01  MIDASI0.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NVD0530L".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  X(72)  VALUE  SPACE.
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  N(1)   VALUE  NC"年"
         CHARACTER     TYPE  IS   NIHONGO.
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  N(1)   VALUE  NC"月"
         CHARACTER     TYPE  IS   NIHONGO.
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  N(1)   VALUE  NC"日"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  PAGE-SUU            PIC  ZZZ9.
     02  FILLER              PIC  N(01)  VALUE  NC"頁"
         CHARACTER     TYPE  IS   NIHONGO.
 01  MIDASI01.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(14)  VALUE
         NC"＜倉庫間在庫移動実績リスト＞"
         CHARACTER     TYPE  IS   YA-22.
     02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  H2-HH               PIC  9(02).
     02  FILLER              PIC  N(1)   VALUE  NC"："
         CHARACTER     TYPE  IS   NIHONGO.
     02  H2-MM               PIC  Z9.
     02  FILLER              PIC  N(1)   VALUE  NC"："
         CHARACTER     TYPE  IS   NIHONGO.
     02  H2-SS               PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKKB            PIC  N(02).
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫：".
     02  H1-SOKCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"日付指定：".
     02  H1-DATKB            PIC  N(03).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-FYY              PIC  9(04).
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  H1-FMM              PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  H1-FDD              PIC  9(02).
     02  FILLER              PIC  N(01)  VALUE  NC"～".
     02  H1-TYY              PIC  9(04).
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  H1-TMM              PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  H1-TDD              PIC  9(02).
****  見出し行２             ****
 01  MIDASI2.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"伝票番号"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"出庫倉庫"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"入庫倉庫"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"備考"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"指示日"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"担当者"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"出庫日"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"担当者"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"入庫日"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"担当者"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"連携"
         CHARACTER     TYPE  IS   YB.
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"商品情報".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"商品名".
     02  FILLER              PIC  X(68)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"出庫_番".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"入庫_番".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"ストック".
     02  FILLER              PIC  X(03)  VALUE  "NO ".
     02  FILLER              PIC  N(02)  VALUE  NC"内訳".
     02  FILLER              PIC  X(65)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"指示数量".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"出庫数量".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"入庫数量".
*
****  見出し行４             ****
 01  MIDASI5        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(68)  VALUE  ALL NC"─".
*
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(02).
     02  M1-DENNO            PIC  9(07).
     02  FILLER              PIC  X(01).
     02  M1-SSOKCD           PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-SSOKNM           PIC  N(10).
     02  FILLER              PIC  X(01).
     02  M1-KAKKS1           PIC  X(01).
     02  M1-SSOKKB           PIC  N(08).
     02  M1-KAKKS2           PIC  X(01).
     02  FILLER              PIC  X(01).
     02  M1-NSOKCD           PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-NSOKNM           PIC  N(10).
     02  FILLER              PIC  X(01).
     02  M1-KAKKA1           PIC  X(01).
     02  M1-NSOKKB           PIC  N(08).
     02  M1-KAKKA2           PIC  X(01).
     02  FILLER              PIC  X(01).
     02  M1-BIKO             PIC  X(10).
     02  FILLER              PIC  X(02).
     02  M1-SDATE            PIC  X(10).
     02  FILLER              PIC  X(01).
     02  M1-STANT            PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-SKDATE           PIC  X(10).
     02  FILLER              PIC  X(01).
     02  M1-SKTANT           PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-NKDATE           PIC  X(10).
     02  FILLER              PIC  X(01).
     02  M1-NKTANT           PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-RENKEI           PIC  N(01).
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(07).
     02  M2-GYONO            PIC  9(02).
     02  FILLER              PIC  X(01).
     02  M2-SYOCD            PIC  X(08).
     02  FILLER              PIC  X(01).
     02  M2-HINCD            PIC  X(10).
     02  FILLER              PIC  X(01).
     02  M2-HINNM1           PIC  N(15).
     02  M2-HINNM2           PIC  N(15).
     02  FILLER              PIC  X(29).
     02  M2-SKTANNO          PIC  X(06).
     02  FILLER              PIC  X(08).
     02  M2-NKTANNO          PIC  X(06).
****  明細行３               ****
 01  MEISAI3.
     02  FILLER              PIC  X(10).
     02  M3-TABLE            OCCURS  5.
         03  M3-STKNO        PIC  X(06).
         03  M3-KAK1         PIC  X(01).
         03  M3-STKSU        PIC  ZZZ,ZZ9.
         03  M3-KAK2         PIC  X(01).
         03  FILLER          PIC  X(01).
     02  M3-SJSURYO          PIC  --,---,--9.99.
     02  FILLER              PIC  X(01).
     02  M3-SKSURYO          PIC  --,---,--9.99.
     02  FILLER              PIC  X(01).
     02  M3-NKSURYO          PIC  --,---,--9.99.
*
****  明細行４               ****
 01  MEISAI4.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### NVD0530L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                   SECTION.
 01  LINK-DATEKBN          PIC X(01).
 01  LINK-SOKCD            PIC X(02).
 01  LINK-FDATE            PIC 9(08).
 01  LINK-TDATE            PIC 9(08).
******************************************************************
 PROCEDURE               DIVISION
                         USING     LINK-DATEKBN
                                   LINK-SOKCD
                                   LINK-FDATE
                                   LINK-TDATE.
******************************************************************
 DECLARATIVES.
*抽出Ｆ
 DNS4-ERR-SEC       SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNSFILF4.
     MOVE    "DNSFILF4"    TO    ERR-FL-ID.
     MOVE     DNS4-STA     TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 DNS5-ERR-SEC       SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNSFILF5.
     MOVE    "DNSFILF5"    TO    ERR-FL-ID.
     MOVE     DNS5-STA     TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 DNS7-ERR-SEC       SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNSFILF7.
     MOVE    "DNSFILF7"    TO    ERR-FL-ID.
     MOVE     DNS7-STA     TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  NVD0530L   START  **"   UPON  CONS.
***************** TEST
*****MOVE  " "        TO  LINK-DATEKBN.
*****MOVE  "  "       TO  LINK-SOKCD.
*****MOVE  "20200101" TO  LINK-FDATE.
*****MOVE  "20200101" TO  LINK-TDATE.
     DISPLAY "LINK-DATEKBN=" LINK-DATEKBN UPON CONS.
     DISPLAY "LINK-SOKCD  =" LINK-SOKCD   UPON CONS.
     DISPLAY "LINK-FDATE  =" LINK-FDATE   UPON CONS.
     DISPLAY "LINK-TDATE  =" LINK-TDATE   UPON CONS.
***************** TEST
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  END-FLG  =  "END".
     PERFORM  END-SEC.
*
     DISPLAY  "**  NVD0530L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     EVALUATE  LINK-DATEKBN
          WHEN SPACE
*指示日
               OPEN     INPUT       DNSFILF4
          WHEN "1"
*出庫日
               OPEN     INPUT       DNSFILF7
          WHEN "2"
*入庫日
               OPEN     INPUT       DNSFILF5
     END-EVALUATE.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     OUTPUT      PRINTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
     ACCEPT   SYS-TIME    FROM  TIME.
*ＭＡＸ行設定
     MOVE     50          TO    MAX-LINE.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-SYSDT            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD(1:4)  TO   H-YY.
     MOVE      LINK-OUT-YMD(5:2)  TO   H-MM.
     MOVE      LINK-OUT-YMD(7:2)  TO   H-DD.
     MOVE      SYS-HH             TO   H2-HH.
     MOVE      SYS-MN             TO   H2-MM.
     MOVE      SYS-SS             TO   H2-SS.
*抽出Ｆリード
     EVALUATE  LINK-DATEKBN
          WHEN SPACE
*指示日
               PERFORM   DNS4-START-SEC
               IF        END-FLG        NOT =   "END"
                  PERFORM     DNS4-READ-SEC
                  MOVE   DNS4-REC           TO  DNS-REC
               END-IF
          WHEN "1"
*出庫日
               PERFORM   DNS7-START-SEC
               IF        END-FLG        NOT =   "END"
                  PERFORM     DNS7-READ-SEC
                  MOVE   DNS7-REC           TO  DNS-REC
               END-IF
          WHEN "2"
*入庫日
               PERFORM   DNS5-START-SEC
               IF        END-FLG        NOT =   "END"
                  PERFORM     DNS5-READ-SEC
                  MOVE   DNS5-REC           TO  DNS-REC
               END-IF
     END-EVALUATE.
     IF        END-FLG            =   "END"
               DISPLAY "対象データはありません" UPON CONS
     ELSE
               MOVE   DNS-F05     TO  WK-SOKCD
               MOVE   99          TO  LINE-CNT
               MOVE   ZERO        TO  PAGE-CNT
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*改ページ
*倉庫ＣＤ ブレイク判定
     IF  LINE-CNT     >=  MAX-LINE
     OR  PAGE-CNT      =  ZERO
     OR  DNS-F05  NOT  =  WK-SOKCD
         MOVE     DNS-F05     TO   WK-SOKCD
         PERFORM  HEAD-WT-SEC
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     EVALUATE  LINK-DATEKBN
          WHEN SPACE
*指示日
               PERFORM     DNS4-READ-SEC
               MOVE   DNS4-REC           TO  DNS-REC
          WHEN "1"
*出庫日
               PERFORM     DNS7-READ-SEC
               MOVE   DNS7-REC           TO  DNS-REC
          WHEN "2"
*入庫日
               PERFORM     DNS5-READ-SEC
               MOVE   DNS5-REC           TO  DNS-REC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PAGE-SUU.
*倉庫
     EVALUATE  LINK-DATEKBN
          WHEN SPACE
*指示日
               MOVE   NC"出庫"       TO  H1-SOKKB
               MOVE   NC"指示日"     TO  H1-DATKB
          WHEN "1"
*出庫日
               MOVE   NC"出庫"       TO  H1-SOKKB
               MOVE   NC"出庫日"     TO  H1-DATKB
          WHEN "2"
*入庫日
               MOVE   NC"入庫"       TO  H1-SOKKB
               MOVE   NC"入庫日"     TO  H1-DATKB
     END-EVALUATE.
     IF    LINK-SOKCD   =  SPACE
           MOVE  NC"全倉庫"          TO  H1-SOKNM
     ELSE
           MOVE  LINK-SOKCD          TO  H1-SOKCD
           MOVE  LINK-SOKCD          TO  SOK-F01
           PERFORM  SOK-READ-RTN
           IF    INV-FLG   =  ZERO
                    MOVE  SOK-F02    TO  H1-SOKNM
           ELSE
                    MOVE  SPACE      TO  H1-SOKNM
           END-IF
     END-IF.
*出庫日
     MOVE     LINK-FDATE(1:4)    TO  H1-FYY.
     MOVE     LINK-FDATE(5:2)    TO  H1-FMM.
     MOVE     LINK-FDATE(7:2)    TO  H1-FDD.
     MOVE     LINK-TDATE(1:4)    TO  H1-TYY.
     MOVE     LINK-TDATE(5:2)    TO  H1-TMM.
     MOVE     LINK-TDATE(7:2)    TO  H1-TDD.
*
     IF       PAGE-CNT   NOT =  1
              MOVE     SPACE     TO  P-REC
              WRITE    P-REC         AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  1.
     WRITE   P-REC     FROM    MIDASI01    AFTER  1.
     WRITE   P-REC     FROM    MIDASI1     AFTER  2.
     WRITE   P-REC     FROM    MEISAI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MEISAI4     AFTER  1.
*
     MOVE  10                    TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
*明細部のクリア
     MOVE  SPACE         TO  MEISAI1  MEISAI2  MEISAI3.
*
*伝票ＮＯ,行_
     MOVE  DNS-F01       TO  M1-DENNO.
     MOVE  DNS-F02       TO  M2-GYONO.
*出庫倉庫,倉庫区分
     MOVE  DNS-F05       TO  M1-SSOKCD.
     MOVE  DNS-F05       TO  SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF    INV-FLG   =  ZERO
           MOVE  SOK-F02    TO  M1-SSOKNM
           MOVE  "("        TO  M1-KAKKS1
           MOVE  ")"        TO  M1-KAKKS2
           IF  SOK-F14      =   SPACE
               MOVE  NC"ＮＡＶＳ倉庫"     TO  M1-SSOKKB
           ELSE
               MOVE  NC"ＳＬＩＭＳ倉庫"   TO  M1-SSOKKB
           END-IF
     ELSE
           MOVE  SPACE      TO  M1-SSOKNM
           MOVE  SPACE      TO  M1-SSOKKB
     END-IF.
*入庫倉庫,倉庫区分
     MOVE  DNS-F08       TO  M1-NSOKCD.
     MOVE  DNS-F08       TO  SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF    INV-FLG   =  ZERO
           MOVE  SOK-F02    TO  M1-NSOKNM
           MOVE  "("        TO  M1-KAKKA1
           MOVE  ")"        TO  M1-KAKKA2
           IF  SOK-F14      =   SPACE
               MOVE  NC"ＮＡＶＳ倉庫"     TO  M1-NSOKKB
           ELSE
               MOVE  NC"ＳＬＩＭＳ倉庫"   TO  M1-NSOKKB
           END-IF
     ELSE
           MOVE  SPACE      TO  M1-NSOKNM
           MOVE  SPACE      TO  M1-NSOKKB
     END-IF.
*備考
     MOVE  DNS-F27          TO  M1-BIKO.
*指示日
     MOVE  DNS-F12(1:4)     TO  M1-SDATE(1:4).
     MOVE  "/"              TO  M1-SDATE(5:1).
     MOVE  DNS-F12(5:2)     TO  M1-SDATE(6:2).
     MOVE  "/"              TO  M1-SDATE(8:1).
     MOVE  DNS-F12(7:2)     TO  M1-SDATE(9:2).
     MOVE  DNS-F11          TO  M1-STANT.
*出庫日
     MOVE  DNS-F14(1:4)     TO  M1-SKDATE(1:4).
     MOVE  "/"              TO  M1-SKDATE(5:1).
     MOVE  DNS-F14(5:2)     TO  M1-SKDATE(6:2).
     MOVE  "/"              TO  M1-SKDATE(8:1).
     MOVE  DNS-F14(7:2)     TO  M1-SKDATE(9:2).
     MOVE  DNS-F16          TO  M1-SKTANT.
*入庫日
     MOVE  DNS-F18(1:4)     TO  M1-NKDATE(1:4).
     MOVE  "/"              TO  M1-NKDATE(5:1).
     MOVE  DNS-F18(5:2)     TO  M1-NKDATE(6:2).
     MOVE  "/"              TO  M1-NKDATE(8:1).
     MOVE  DNS-F18(7:2)     TO  M1-NKDATE(9:2).
     MOVE  DNS-F20          TO  M1-NKTANT.
*連携
     IF    DNS-F22    =   SPACE
           MOVE  NC"未"     TO  M1-RENKEI
     ELSE
           MOVE  NC"済"     TO  M1-RENKEI
     END-IF
*商品ＣＤ
     MOVE  DNS-F03          TO  M2-SYOCD.
*品単ＣＤ
     MOVE  DNS-F04(1:5)     TO  M2-HINCD(1:5).
     MOVE  "-"              TO  M2-HINCD(6:1).
     MOVE  DNS-F04(6:2)     TO  M2-HINCD(7:2).
     MOVE  "-"              TO  M2-HINCD(9:1).
     MOVE  DNS-F04(8:1)     TO  M2-HINCD(10:1).
*商品名
     MOVE  DNS-F03          TO  MEI-F011.
     MOVE  DNS-F04          TO  MEI-F012.
     PERFORM  MEI-READ-RTN.
     IF    INV-FLG  =  ZERO
           MOVE  MEI-F021   TO  M2-HINNM1
           MOVE  MEI-F022   TO  M2-HINNM2
     ELSE
           MOVE  SPACE      TO  M2-HINNM1
                                M2-HINNM2
     END-IF.
*_番
     MOVE  DNS-F07          TO  M2-SKTANNO.
     MOVE  DNS-F10          TO  M2-NKTANNO.
*ストック
     PERFORM  VARYING  IX1  FROM  1  BY  1
              UNTIL    IX1  >     5
           MOVE  DNS-F25(IX1)     TO  M3-STKNO(IX1)
           MOVE  "("              TO  M3-KAK1 (IX1)
           MOVE  DNS-F28(IX1)     TO  M3-STKSU(IX1)
           MOVE  ")"              TO  M3-KAK2 (IX1)
     END-PERFORM.
*指示数量
     MOVE  DNS-F13           TO  M3-SJSURYO.
*出庫数量
     MOVE  DNS-F15           TO  M3-SKSURYO.
*入庫数量
     MOVE  DNS-F19           TO  M3-NKSURYO.
*印刷
     WRITE    P-REC         FROM    MEISAI1    AFTER  2
     WRITE    P-REC         FROM    MEISAI2    AFTER  1.
     WRITE    P-REC         FROM    MEISAI3    AFTER  1.
     ADD   4                     TO  LINE-CNT.
*
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*    指示日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS4-START-SEC      SECTION.
     INITIALIZE                  DNS4-REC.
     IF    LINK-SOKCD    NOT =   SPACE
           MOVE  LINK-SOKCD  TO  DNS4-F05
     END-IF.
*
     START DNSFILF4  KEY >=      DNS4-F05  DNS4-F12
                                 DNS4-F01  DNS4-F02
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 DNS4-START-EXIT.
     EXIT.
*=============================================================
*    指示日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS4-READ-SEC    SECTION.
*リード
 DNS4-010.
     READ  DNSFILF4     NEXT
         AT   END
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS4-READ-EXIT
     END-READ.
*カウント
     ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (LINK-SOKCD  NOT =   SPACE   )  AND
          (LINK-SOKCD  NOT =   DNS4-F05)
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS4-READ-EXIT
     END-IF.
*指定日判定
     IF  (LINK-FDATE       >   DNS4-F12  OR
          LINK-TDATE       <   DNS4-F12)
           GO              TO  DNS4-010
     END-IF.
*
 DNS4-READ-EXIT.
     EXIT.
*=============================================================
*    入庫日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS5-START-SEC      SECTION.
     INITIALIZE                  DNS5-REC.
     IF    LINK-SOKCD    NOT =   SPACE
           MOVE  LINK-SOKCD  TO  DNS5-F08
     END-IF.
*
     START DNSFILF5  KEY >=      DNS5-F08  DNS5-F18
                                 DNS5-F01  DNS5-F02
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 DNS5-START-EXIT.
     EXIT.
*=============================================================
*    入庫日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS5-READ-SEC    SECTION.
*リード
 DNS5-010.
     READ  DNSFILF5     NEXT
         AT   END
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS5-READ-EXIT
     END-READ.
*カウント
     ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (LINK-SOKCD  NOT =   SPACE   )  AND
          (LINK-SOKCD  NOT =   DNS5-F08)
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS5-READ-EXIT
     END-IF.
*入庫日判定
     IF  (LINK-FDATE       >   DNS5-F18  OR
          LINK-TDATE       <   DNS5-F18)
           GO              TO  DNS5-010
     END-IF.
*
 DNS5-READ-EXIT.
     EXIT.
*=============================================================
*    出庫日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS7-START-SEC      SECTION.
     INITIALIZE                  DNS7-REC.
     IF    LINK-SOKCD    NOT =   SPACE
           MOVE  LINK-SOKCD  TO  DNS7-F05
     END-IF.
*
     START DNSFILF7  KEY >=      DNS7-F05  DNS7-F14
                                 DNS7-F01  DNS7-F02
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 DNS7-START-EXIT.
     EXIT.
*=============================================================
*    出庫日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS7-READ-SEC    SECTION.
*リード
 DNS7-010.
     READ  DNSFILF7     NEXT
         AT   END
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS7-READ-EXIT
     END-READ.
*カウント
     ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (LINK-SOKCD  NOT =   SPACE   )  AND
          (LINK-SOKCD  NOT =   DNS7-F05)
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS7-READ-EXIT
     END-IF.
*出庫日判定
     IF  (LINK-FDATE       >   DNS7-F14  OR
          LINK-TDATE       <   DNS7-F14)
           GO              TO  DNS7-010
     END-IF.
*
 DNS7-READ-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     EVALUATE  LINK-DATEKBN
          WHEN SPACE
*指示日
               CLOSE    DNSFILF4
          WHEN "1"
*出庫日
               CLOSE    DNSFILF7
          WHEN "2"
*入庫日
               CLOSE    DNSFILF5
     END-EVALUATE.
     CLOSE      PRINTF.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
*ＭＳＧ出力
     DISPLAY "* DNSFILF (IN)=" READ-CNT " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     MEIMS1    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SOK-READ-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZSOKMS1   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.

```
