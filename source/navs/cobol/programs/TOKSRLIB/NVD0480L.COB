****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　倉庫間在庫移動入力チェックリスト　*
*    作成日／作成者　　　：　2020/04/09   ASS.II               *
*    処理内容　　　　　　：　新入出庫ファイルより倉庫間在庫移動*
*    　　　　　　　　　　　　入力チェックリストを出力する。　　*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         NVD0480L.
 AUTHOR.             ASS.II.
 DATE-WRITTEN.       02.04.09.
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
     SELECT   DNSFILF   ASSIGN        TO  DA-01-VI-DNSFILL6
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS-F05  DNS-F11
                                          DNS-F12  DNS-F01
                                          DNS-F02
                        FILE STATUS   IS  DNS-STA.

*担当者Ｍ
     SELECT   HTANMS    ASSIGN        TO  DA-01-VI-TANMS1
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  RANDOM
                        RECORD KEY    IS  TAN-F01
                                          TAN-F02
                        FILE STATUS   IS  TAN-STA.
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
 FD  DNSFILF            LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS  AS PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1            LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*商品名称Ｍ
 FD  MEIMS1             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*担当者Ｍ
 FD  HTANMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTANMS    OF   XFDLIB    JOINING   TAN  AS PREFIX.
*プリンタＦ
 FD  PRINTF.
 01    P-REC                 PIC  X(200).
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  DNS-STA             PIC  X(02).
     03  TAN-STA             PIC  X(02).
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
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
****  見出し行０             ****
 01  MIDASI0.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NVD0480L".
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
     02  FILLER              PIC  N(18)  VALUE
         NC"＜倉庫間在庫移動入力チェックリスト＞"
         CHARACTER     TYPE  IS   YA-22.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  H2-HH               PIC  9(02).
     02  FILLER              PIC  N(1)   VALUE  NC"："
         CHARACTER     TYPE  IS   NIHONGO.
     02  H2-MM               PIC  Z9.
     02  FILLER              PIC  N(1)   VALUE  NC"："
         CHARACTER     TYPE  IS   NIHONGO.
     02  H2-SS               PIC  Z9.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"出庫倉庫：".
     02  H1-SOKCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"指示者：".
     02  H1-TANCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-TANNM            PIC  N(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"指示日：".
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
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"伝票".
     02  FILLER              PIC  X(02)  VALUE  "NO".
     02  FILLER              PIC  N(01)  VALUE  NC"：".
     02  H1-DENST            PIC  9(07).
     02  FILLER              PIC  N(01)  VALUE  NC"～".
     02  H1-DENED            PIC  9(07).
****  見出し行２             ****
 01  MIDASI2.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"伝票番号"
         CHARACTER     TYPE  IS   YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"出庫倉庫"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"相手倉庫"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"担当者"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"指示日"
         CHARACTER     TYPE  IS   NIHONGO.
****  見出し行３             ****
 01  MIDASI3.
     02  FILLER              PIC  X(05)  VALUE SPACE.
     02  FILLER              PIC  N(02)  VALUE NC"　行"
         CHARACTER     TYPE  IS   YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"_番"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"商品情報"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(17)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"商品名"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(45)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"移動数量"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"備考"
         CHARACTER     TYPE  IS   NIHONGO.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"相手_番"
         CHARACTER     TYPE   IS  YB.
*
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
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
     02  M1-ASOKCD           PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-ASOKNM           PIC  N(10).
     02  FILLER              PIC  X(01).
     02  M1-KAKKA1           PIC  X(01).
     02  M1-ASOKKB           PIC  N(08).
     02  M1-KAKKA2           PIC  X(01).
     02  FILLER              PIC  X(01).
     02  M1-TANCD            PIC  X(02).
     02  FILLER              PIC  X(01).
     02  M1-TANNM            PIC  N(10).
     02  FILLER              PIC  X(09).
     02  M1-SDATE            PIC  X(10).
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(07).
     02  M2-GYONO            PIC  9(02).
     02  FILLER              PIC  X(01).
     02  M2-TANNO            PIC  X(06).
     02  FILLER              PIC  X(03).
     02  M2-SYOCD            PIC  X(08).
     02  FILLER              PIC  X(01).
     02  M2-HINCD            PIC  X(10).
     02  FILLER              PIC  X(06).
     02  M2-HINNM1           PIC  N(15).
     02  M2-HINNM2           PIC  N(15).
     02  FILLER              PIC  X(01).
     02  M2-SURYO            PIC  --,---,--9.99.
     02  FILLER              PIC  X(02).
     02  M2-BIKOU            PIC  X(10).
     02  FILLER              PIC  X(01).
     02  M2-ATANNO           PIC  X(06).
*
****  明細行３               ****
 01  MEISAI3.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### NVD0480L".
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
 01  LINK-BUMON            PIC X(04).
 01  LINK-TANCD            PIC X(02).
 01  LINK-SOKCD            PIC X(02).
 01  LINK-FDATE            PIC 9(08).
 01  LINK-TDATE            PIC 9(08).
 01  LINK-DENST            PIC 9(07).
 01  LINK-DENED            PIC 9(07).
******************************************************************
 PROCEDURE               DIVISION
                         USING     LINK-BUMON
                                   LINK-TANCD
                                   LINK-SOKCD
                                   LINK-FDATE
                                   LINK-TDATE
                                   LINK-DENST
                                   LINK-DENED.
******************************************************************
 DECLARATIVES.
*抽出Ｆ
 DNS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNSFILF.
     MOVE    "DNSFILF"     TO    ERR-FL-ID.
     MOVE     DNS-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*担当者Ｍ
 TAN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HTANMS.
     MOVE    "HTANMS"      TO    ERR-FL-ID.
     MOVE     TAN-STA      TO    ERR-STCD.
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
     DISPLAY  "**  NVD0480L   START  **"   UPON  CONS.
***************** TEST
*****MOVE  "2920"     TO  LINK-BUMON
*****MOVE  "  "       TO  LINK-TANCD.
*****MOVE  "  "       TO  LINK-SOKCD.
*****MOVE  "00000000" TO  LINK-FDATE.
*****MOVE  "99999999" TO  LINK-TDATE.
*****MOVE  "0000000"  TO  LINK-DENST.
*****MOVE  "9999999"  TO  LINK-DENED.
     DISPLAY "LINK-BUMON=" LINK-BUMON UPON CONS.
     DISPLAY "LINK-TANCD=" LINK-TANCD UPON CONS.
     DISPLAY "LINK-SOKCD=" LINK-SOKCD UPON CONS.
     DISPLAY "LINK-FDATE=" LINK-FDATE UPON CONS.
     DISPLAY "LINK-TDATE=" LINK-TDATE UPON CONS.
     DISPLAY "LINK-DENST=" LINK-DENST UPON CONS.
     DISPLAY "LINK-DENED=" LINK-DENED UPON CONS.
***************** TEST
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  END-FLG  =  "END".
     PERFORM  END-SEC.
*
     DISPLAY  "**  NVD0480L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     INPUT       DNSFILF.
     OPEN     INPUT       HTANMS.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     OUTPUT      PRINTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
     ACCEPT   SYS-TIME    FROM  TIME.
*ＭＡＸ行設定
     MOVE     51          TO    MAX-LINE.
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
     PERFORM   DNS-START-SEC.
     IF        END-FLG        NOT =   "END"
        PERFORM     DNS-READ-SEC
        MOVE   DNS-F05            TO  WK-SOKCD
        MOVE   99                 TO  LINE-CNT
        MOVE   ZERO               TO  PAGE-CNT
     END-IF.
     IF        END-FLG            =   "END"
               DISPLAY "対象データはありません" UPON CONS
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
     PERFORM  DNS-READ-SEC.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PAGE-SUU.
*倉庫
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
*指示者
     IF    LINK-TANCD   =  SPACE
           MOVE  NC"全担当"          TO  H1-TANNM
     ELSE
           MOVE  LINK-TANCD          TO  H1-TANCD
           MOVE  LINK-TANCD          TO  TAN-F02
           PERFORM  TAN-READ-RTN
           IF    INV-FLG   =  ZERO
                    MOVE  TAN-F03    TO  H1-TANNM
           ELSE
                    MOVE  SPACE      TO  H1-TANNM
           END-IF
     END-IF.
*指示日
     MOVE     LINK-FDATE(1:4)    TO  H1-FYY.
     MOVE     LINK-FDATE(5:2)    TO  H1-FMM.
     MOVE     LINK-FDATE(7:2)    TO  H1-FDD.
     MOVE     LINK-TDATE(1:4)    TO  H1-TYY.
     MOVE     LINK-TDATE(5:2)    TO  H1-TMM.
     MOVE     LINK-TDATE(7:2)    TO  H1-TDD.
*伝票NO
     MOVE     LINK-DENST         TO  H1-DENST.
     MOVE     LINK-DENED         TO  H1-DENED.
*
     IF       PAGE-CNT   NOT =  1
              MOVE     SPACE     TO  P-REC
              WRITE    P-REC         AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  1.
     WRITE   P-REC     FROM    MIDASI01    AFTER  1.
     WRITE   P-REC     FROM    MIDASI1     AFTER  2.
     WRITE   P-REC     FROM    MEISAI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MEISAI3     AFTER  1.
*
     MOVE  10                    TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
*明細部のクリア
     MOVE  SPACE         TO  MEISAI1  MEISAI2.
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
*相手倉庫,倉庫区分
     MOVE  DNS-F08       TO  M1-ASOKCD.
     MOVE  DNS-F08       TO  SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF    INV-FLG   =  ZERO
           MOVE  SOK-F02    TO  M1-ASOKNM
           MOVE  "("        TO  M1-KAKKA1
           MOVE  ")"        TO  M1-KAKKA2
           IF  SOK-F14      =   SPACE
               MOVE  NC"ＮＡＶＳ倉庫"     TO  M1-ASOKKB
           ELSE
               MOVE  NC"ＳＬＩＭＳ倉庫"   TO  M1-ASOKKB
           END-IF
     ELSE
           MOVE  SPACE      TO  M1-ASOKNM
           MOVE  SPACE      TO  M1-ASOKKB
     END-IF.
*担当者
     MOVE  DNS-F11          TO  M1-TANCD.
     MOVE  DNS-F11          TO  TAN-F02.
     PERFORM  TAN-READ-RTN.
     IF    INV-FLG   =  ZERO
              MOVE  TAN-F03    TO  M1-TANNM
     ELSE
              MOVE  SPACE      TO  M1-TANNM
     END-IF.
*指示日
     MOVE  DNS-F12(1:4)     TO  M1-SDATE(1:4).
     MOVE  "/"              TO  M1-SDATE(5:1).
     MOVE  DNS-F12(5:2)     TO  M1-SDATE(6:2).
     MOVE  "/"              TO  M1-SDATE(8:1).
     MOVE  DNS-F12(7:2)     TO  M1-SDATE(9:2).
*_番
     MOVE  DNS-F07          TO  M2-TANNO.
*商品ＣＤ
     MOVE  DNS-F03          TO  M2-SYOCD.
*品単ＣＤ
     MOVE  DNS-F04(1:5)     TO  M2-HINCD(1:5).
     MOVE  "-"              TO  M2-HINCD(6:1).
     MOVE  DNS-F04(6:2)     TO  M2-HINCD(7:2).
     MOVE  "-"              TO  M2-HINCD(9:1).
     MOVE  DNS-F04(8:1)     TO  M2-HINCD(10:1).
*商品名
     MOVE  DNS-F03           TO  MEI-F011.
     MOVE  DNS-F04           TO  MEI-F012.
     PERFORM  MEI-READ-RTN.
     IF    INV-FLG  =  ZERO
              MOVE  MEI-F021     TO  M2-HINNM1
              MOVE  MEI-F022     TO  M2-HINNM2
     ELSE
              MOVE  SPACE        TO  M2-HINNM1
                                     M2-HINNM2
     END-IF.
*数量
     MOVE  DNS-F13           TO  M2-SURYO.
*備考
     MOVE  DNS-F27           TO  M2-BIKOU.
*相手_番
     MOVE  DNS-F10           TO  M2-ATANNO.
*印刷
     WRITE    P-REC         FROM    MEISAI1    AFTER  2
     WRITE    P-REC         FROM    MEISAI2    AFTER  1.
     ADD   3                     TO  LINE-CNT.
*
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS-START-SEC      SECTION.
     INITIALIZE                  DNS-REC.
     IF    LINK-SOKCD    NOT =   SPACE
           MOVE  LINK-SOKCD  TO  DNS-F05
     END-IF.
*
     START DNSFILF   KEY >=      DNS-F05  DNS-F11  DNS-F12
                                 DNS-F01  DNS-F02
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 DNS-START-EXIT.
     EXIT.
*=============================================================
*                新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS-READ-SEC    SECTION.
*リード
 DNS-010.
     READ  DNSFILF      NEXT
         AT   END
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS-READ-EXIT
     END-READ.
*カウント
     ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (LINK-SOKCD  NOT =   SPACE  )  AND
          (LINK-SOKCD  NOT =   DNS-F05)
           MOVE   "END"    TO  END-FLG
           GO              TO  DNS-READ-EXIT
     END-IF.
*担当者ＣＤ判定
     IF   (LINK-TANCD  NOT =   SPACE  )  AND
          (LINK-TANCD  NOT =   DNS-F11)
           GO              TO  DNS-010
     END-IF.
*指示日判定
     IF  (LINK-FDATE       >   DNS-F12  OR
          LINK-TDATE       <   DNS-F12)
           GO              TO  DNS-010
     END-IF.
*伝票番号判定
     IF  (LINK-DENST       >   DNS-F01  OR
          LINK-DENED       <   DNS-F01)
           GO              TO  DNS-010
     END-IF.
*
 DNS-READ-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      PRINTF.
     CLOSE      DNSFILF.
     CLOSE      HTANMS.
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
*--------------------------------------------------------------*
*    LEVEL  ALL   担当者マスタ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 TAN-READ-RTN         SECTION.
     MOVE   0           TO   INV-FLG.
     MOVE   LINK-BUMON  TO   TAN-F01.
     READ   HTANMS      INVALID   KEY
            MOVE        1         TO   INV-FLG
     END-READ.
 TAN-READ-EXIT.
     EXIT.
