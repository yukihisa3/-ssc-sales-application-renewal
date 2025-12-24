# NVD0151L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0151L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定表発行　　　　　　　　　　*
*    作成日／作成者　　　：　2020/02/07   ASS.II               *
*    処理内容　　　　　　：　入荷予定リストWKを順読みし、　
*    　　　　　　　　　　　　各マスタを参照しながら、　　　　　*
*                            入荷予定表を出力する。　　　　　　*
*    更新日／更新者　　　：　2022/07/20   INOUE                *
*    更新内容　　　　　　：　ヘッダ：商品カテゴリ追加　　　　
*    　　　　　　　　　　　　改頁条件追加　　　　　　　　　　
*    　　　　　　　　　　　　商品名称Ｍ→ＳＵＢ商品名称Ｍ
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         NVD0151L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       20.02.07.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*抽出Ｆ
     SELECT   NYYOTEIF       ASSIGN        TO  DA-01-S-NYYOTEIF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  NYY-STA.

*仕入先Ｍ
     SELECT   ZSHIMS1        ASSIGN        TO  DA-01-VI-ZSHIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHI-F01
                             FILE STATUS   IS  SHI-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  DA-01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
     SELECT   SUBMEIL1       ASSIGN        TO  DA-01-VI-SUBMEIL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  MEI-F011
                                               MEI-F012
                             FILE STATUS   IS  MEI-STA.
*条件ファイル
     SELECT   JYOKEN1        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.
*プリンタＦ
     SELECT   PRINTF    ASSIGN  TO   LP-04.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*抽出Ｆ
 FD  NYYOTEIF
     LABEL       RECORD    IS        STANDARD
                 BLOCK  CONTAINS  6    RECORDS.
     COPY     NYYOTEIF OF  XFDLIB
     JOINING  NYY      AS  PREFIX.
*仕入先Ｍ
 FD  ZSHIMS1
     LABEL       RECORD    IS        STANDARD.
     COPY     ZSHIMS   OF  XFDLIB
     JOINING  SHI      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1
     LABEL       RECORD    IS        STANDARD.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  SUBMEIL1
     LABEL       RECORD    IS        STANDARD.
     COPY     SUBMEIF   OF  XFDLIB
     JOINING  MEI      AS  PREFIX.
*条件ファイル
 FD  JYOKEN1
     LABEL       RECORD    IS        STANDARD.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*プリンタＦ
 FD  PRINTF.
 01    P-REC                 PIC  X(200).
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  NYY-STA             PIC  X(02).
     03  SHI-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  MEI-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER              PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN          PIC  N(06)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"）".
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  HAC-FLG             PIC  9(01)  VALUE  ZERO.
     03  SIR-FLG             PIC  9(01)  VALUE  ZERO.
     03  DEN-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-SIRCD            PIC  X(08)  VALUE  SPACE.
     03  WK-SYBCD            PIC  X(02)  VALUE  SPACE.
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
*20220720
     03  WK-CATEGORY         PIC  X(02)  VALUE  SPACE.
     03  WK-DENNO            PIC  9(07)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
     03  WK-MEMO             PIC  X(07)  VALUE  SPACE.
****  見出し行０             ****
 01  MIDASI0        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NVD0151L".
     02  FILLER              PIC  X(25)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"※※　入　荷　予　定　リ　ス　ト　※※".
     02  H-TOKHAN            PIC  N(08)  VALUE  SPACE.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZZ9.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫：".
     02  H1-SOKCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  H1-SOKNM            PIC  N(18).
     02  H1-SOKNM            PIC  N(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"入荷種別：".
     02  H1-SYBCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SYBNM            PIC  N(10).
*20220720
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(07)
                                  VALUE  NC"商品カテゴリ：".
     02  H1-CATEGORY         PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-CATEGORYNM       PIC  N(05).
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕入先".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注_".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"入荷予定日".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"商品".
     02  FILLER              PIC  X(06)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(42)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"入荷予定数".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"備　考".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"メモ".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"仕入先名".
*--  02  FILLER              PIC  X(45)  VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"（　".
     02  FILLER              PIC  X(04)  VALUE  "D365".
     02  FILLER              PIC  N(06)  VALUE
         NC"発注番号　）".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"社外ＭＳＧ".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT01               PIC  X(08)B.
     02  PRT02               PIC  9(07)BB.
     02  PRT03-YY            PIC  9(02).
     02  P1                  PIC  X(01).
     02  PRT03-MM            PIC  9(02).
     02  P2                  PIC  X(01).
     02  PRT03-DD            PIC  9(02)BB.
     02  PRT04               PIC  9(01)BB.
     02  PRT05               PIC  X(08)B.
     02  PRT06               PIC  X(08)B.
     02  PRT071              PIC  N(15).
     02  PRT072              PIC  N(15).
     02  PRT08               PIC  -,---,--9.99B.
     02  PRT091              PIC  X(17).
     02  PRT092              PIC  X(01)B.
*#2021/06/16 NAV ST
*****02  PRT10               PIC  X(07).
     02  PRT10               PIC  X(07)B.
     02  PRT11               PIC  N(02).
*#2021/06/16 NAV ED
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT21               PIC  N(18).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "(".
     02  PRT2-HACNO          PIC  X(20).
     02  FILLER              PIC  X(01)  VALUE  ")".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT22               PIC  X(02)B.
     02  PRT23               PIC  N(20).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### NVD0151L".
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
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD.
******************************************************************
 DECLARATIVES.
*抽出Ｆ
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYYOTEIF.
     MOVE    "NYYOTEIF"    TO    ERR-FL-ID.
     MOVE     NYY-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*仕入先Ｍ
 SHI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSHIMS1.
     MOVE    "ZSHIMS1"     TO    ERR-FL-ID.
     MOVE     SHI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       SUBMEIL1.
     MOVE    "SUBMEIL1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*条件ファイル
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JYOKEN1.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  NVD0151L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  NVD0151L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     INPUT       NYYOTEIF.
     OPEN     INPUT       ZSHIMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       SUBMEIL1.
     OPEN     INPUT       JYOKEN1.
     OPEN     OUTPUT      PRINTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
*ＭＡＸ行設定
*#2021/06/10 NAV ST
*****MOVE     62          TO    MAX-LINE.
     MOVE     56          TO    MAX-LINE.
*#2021/06/10 NAV ED
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
*20220720
*特販部名称編集
*-   MOVE      SPACE              TO   JYO-REC.
*-   INITIALIZE                        JYO-REC.
*-   MOVE     "99"                TO   JYO-F01.
*-   MOVE     "BUMON"             TO   JYO-F02.
*-   READ      JYOKEN1
*-     INVALID KEY
*-             MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
*-     NOT INVALID KEY
*-             MOVE JYO-F03            TO   HEN-TOKHAN
*-   END-READ.
*-   MOVE      HEN-TOKHAN-AREA    TO   H-TOKHAN.
*抽出Ｆリード
     PERFORM  NYY-RD-SEC.
     IF  HAC-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*改ページ
*倉庫ＣＤ ブレイク判定
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  NYY-F01  NOT  =  H1-SOKCD
     OR  NYY-F12  NOT  =  H1-SYBCD
*20220720
     OR  NYY-F14  NOT  =  H1-CATEGORY
         MOVE     NYY-F01     TO   WK-SOKOCD
         MOVE     NYY-F12     TO   WK-SYBCD
*20220720
         MOVE     NYY-F14     TO   WK-CATEGORY
         MOVE     NYY-F02     TO   WK-SIRCD
         MOVE     NYY-F03     TO   WK-DENNO
         PERFORM  HEAD-WT-SEC
         MOVE     1           TO   SIR-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*仕入先ＣＤ ブレイク判定
     IF  NYY-F02  NOT  =  WK-SIRCD
         MOVE     NYY-F02     TO   WK-SIRCD
         MOVE     NYY-F03     TO   WK-DENNO
         MOVE     1           TO   SIR-FLG
         MOVE     1           TO   DEN-FLG
     END-IF.
*伝票_ ブレイク判定
     IF  NYY-F03     NOT  =  WK-DENNO
         MOVE     NYY-F03     TO   WK-DENNO
         MOVE     1           TO   DEN-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     PERFORM  NYY-RD-SEC.
     IF  HAC-FLG   =  9    OR   NYY-F02  NOT =  WK-SIRCD
         MOVE   SPACE                TO  P-REC
         WRITE  P-REC             AFTER  1
         ADD    1                    TO  LINE-CNT
     END-IF.
     IF  HAC-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PAGE-SUU.
*倉庫
     MOVE  WK-SOKOCD             TO  H1-SOKCD.
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE            TO  H1-SOKNM
           NOT INVALID  KEY
              MOVE  SOK-F02          TO  H1-SOKNM
     END-READ.
*種別
     MOVE  WK-SYBCD              TO  H1-SYBCD.
     MOVE  87                    TO  JYO-F01.
     MOVE  WK-SYBCD              TO  JYO-F02.
     READ      JYOKEN1
       INVALID KEY
               MOVE NC"＊＊＊＊＊＊"   TO   H1-SYBNM
       NOT INVALID KEY
               MOVE JYO-F03            TO   H1-SYBNM
     END-READ.
*20220720
*商品カテゴリ
     MOVE  WK-CATEGORY           TO  H1-CATEGORY.
     MOVE  10                    TO  JYO-F01.
     MOVE  WK-CATEGORY           TO  JYO-F02.
     READ      JYOKEN1
       INVALID KEY
               MOVE NC"＊＊＊＊＊＊"   TO   H1-CATEGORYNM
       NOT INVALID KEY
               MOVE JYO-F03            TO   H1-CATEGORYNM
     END-READ.
*印刷
     IF      PAGE-CNT    NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  2.
     WRITE   P-REC     FROM    MIDASI1     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
*
     MOVE  6                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
*明細部のクリア
     MOVE  SPACE         TO  MEISAI1  MEISAI2.
*仕入先
     IF    SIR-FLG    =   1
       MOVE  NYY-F02     TO  PRT01
       MOVE  NYY-F02     TO  SHI-F01
       READ  ZSHIMS1
             INVALID  KEY
                MOVE  SPACE       TO  PRT21
             NOT INVALID  KEY
                MOVE  SHI-F02     TO  PRT21
       END-READ
       MOVE  ZERO        TO  SIR-FLG
     END-IF.
*伝票_
     IF    DEN-FLG    =   1
       MOVE  NYY-F03         TO  PRT02
*伝票番号
       MOVE  NYY-F13         TO  PRT2-HACNO
*社外ＭＳＧ
       MOVE  NYY-F111        TO  PRT22
       MOVE  NYY-F112        TO  PRT23
*入荷予定日
       MOVE  NYY-F05(3:2)    TO  PRT03-YY
       MOVE  NYY-F05(5:2)    TO  PRT03-MM
       MOVE  NYY-F05(7:2)    TO  PRT03-DD
       MOVE  "."             TO  P1  P2
*メモ
       MOVE  NYY-F10         TO  PRT10
       MOVE  ZERO            TO  DEN-FLG
     END-IF.
*#2021/06/15 NAV ST
     IF   NYY-F99(22:1)  =  "1"
          MOVE NC"分納"      TO  PRT11
     ELSE
          MOVE SPACE         TO  PRT11
     END-IF.
*#2021/06/15 NAV ED
*行_
     MOVE  NYY-F04           TO  PRT04.
*商品ＣＤ
     MOVE  NYY-F06           TO  PRT05.
*商品名
     MOVE  NYY-F06           TO  MEI-F011.
     MOVE  NYY-F07           TO  MEI-F012.
     READ  SUBMEIL1
           INVALID  KEY
              MOVE  SPACE        TO  PRT071  PRT072
           NOT INVALID  KEY
              MOVE  MEI-F021     TO  PRT071
              MOVE  MEI-F022     TO  PRT072
     END-READ.
*_番
     IF    NYY-F08  NOT = SPACE
       MOVE  NYY-F08(1:1)        TO  PRT06(1:1)
       MOVE  "-"                 TO  PRT06(2:1)
       MOVE  NYY-F08(2:3)        TO  PRT06(3:3)
       MOVE  "-"                 TO  PRT06(6:1)
       MOVE  NYY-F08(5:2)        TO  PRT06(7:2)
     END-IF.
*入荷予定数
     MOVE  NYY-F09           TO  PRT08.
*備考
     MOVE  "("               TO  PRT091.
     MOVE  ")"               TO  PRT092.
*印刷
     WRITE    P-REC         FROM    MEISAI1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI2    AFTER  1.
*
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                抽出Ｆリード処理
*=============================================================
 NYY-RD-SEC      SECTION.
*リード
     READ  NYYOTEIF      NEXT
         AT   END
           MOVE   9        TO  HAC-FLG
           GO              TO  NYY-RD-EXIT
     END-READ.
*カウント
     ADD   1               TO         IN-CNT.
 NYY-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      PRINTF.
     CLOSE      NYYOTEIF.
     CLOSE      ZSHIMS1.
     CLOSE      ZSOKMS1.
     CLOSE      SUBMEIL1.
     CLOSE      JYOKEN1.
*ＭＳＧ出力
     DISPLAY "* NYYOTEIF(IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.

```
