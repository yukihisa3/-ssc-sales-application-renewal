# SSM0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSM0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　出荷業務　出荷明細書　　　　　　　*
*    モジュール名　　　　：　出荷明細データ抽出　　　　　      *
*    作成日／更新日　　　：　2018/08/27                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*    更新日／更新者　　　：　2021/04/06 NAV TAKAHASHI          *
*    修正概要　　　　　　：　ＤＣＭ仕入先統合                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSM0020B.
 AUTHOR.                NAV T.TAKAHASHI.
 DATE-WRITTEN.          18/08/27.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*出荷明細データ（ケース数算出）
     SELECT   SYKMEIF  ASSIGN     TO        DA-01-VI-SYKMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYK-F01   SYK-F02
                                            SYK-F04   SYK-F03
                                            SYK-F18   SYK-F23
                                            SYK-F24   SYK-F25
                                            SYK-F05   SYK-F07
                        FILE      STATUS    IS   SYK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*サブ商品変換テーブル
     SELECT   SUBTBLL1   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F01   SUB-F02
                        FILE      STATUS    IS   SUB-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷明細ワークファイル
******************************************************************
 FD  SYKMEIF           LABEL RECORD   IS   STANDARD.
     COPY     SYKMEIF  OF        XFDLIB
              JOINING  SYK       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    サブ商品変換テーブル
******************************************************************
 FD  SUBTBLL1            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
******************************************************************
*    サブ商品変換テーブル
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*ホーマック
*#2021/04/06 NAV ST
*****COPY   HMSHIRED  OF  XFDLIB  JOINING  HMC  AS   PREFIX.
     COPY   DCMDENF   OF  XFDLIB  JOINING  HMC  AS   PREFIX.
*#2021/04/06 NAV ED
*カーマ／ダイキ／ケーヨーの場合
*#2021/04/06 NAV ST
*****COPY   KMSHIRED  OF  XFDLIB  JOINING  KMA  AS   PREFIX.
     COPY   DCMDENF   OF  XFDLIB  JOINING  KMA  AS   PREFIX.
*#2021/04/06 NAV ED
*エンチョーの場合
     COPY   ENSHIREN  OF  XFDLIB  JOINING  ENC  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SUBTBLL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-DEN-F49              PIC  X(06)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SYK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  SUB-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSM0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSM0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSM0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*税計算用
 01  WK-ZEIRITU1             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU2             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIKAITEI            PIC  9(08)     VALUE  ZERO.
 01  WK-ZEIRITU3             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU4             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIKOMI              PIC  9(09)     VALUE  ZERO.
*ケース計算用
 01  WK-SYO                  PIC  9(09)     VALUE  ZERO.
 01  WK-AMARI                PIC  9(09)     VALUE  ZERO.
 01  WK-IRISU                PIC  9(06)     VALUE  ZERO.
*日付チェック用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-JTOKCD            PIC   9(08).
 01  PARA-JSOKCD            PIC   X(02).
 01  PARA-JNOUDT            PIC   9(08).
 01  PARA-JTENST            PIC   9(05).
 01  PARA-JTENED            PIC   9(05).
 01  PARA-JBUMST            PIC   X(04).
 01  PARA-JBUMED            PIC   X(04).
 01  PARA-JTANAS            PIC   X(06).
 01  PARA-JTANAE            PIC   X(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-JTOKCD
                                       PARA-JSOKCD
                                       PARA-JNOUDT
                                       PARA-JTENST
                                       PARA-JTENED
                                       PARA-JBUMST
                                       PARA-JBUMED
                                       PARA-JTANAS
                                       PARA-JTANAE.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SYKMEIF.
     MOVE      "SYKMEIF "   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1  "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLL1.
     MOVE      "SUBTBLL1  "   TO   AB-FILE.
     MOVE      SUB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1   "  TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1"    TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLA  SHOTBL1  SUBTBLL1  MEIMS1
                        JYOKEN1   TOKMS2.
     OPEN     OUTPUT    SYKMEIF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-JTOKCD    TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*    条件ファイル検索
     MOVE     SPACE          TO   JYO-REC
     INITIALIZE                   JYO-REC
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     READ     JYOKEN1
         INVALID
*             消費税税率が取得出来ない場合は異常終了
              DISPLAY NC"消費税率取得エラー！！" UPON CONS
              MOVE 4000      TO   PROGRAM-STATUS
              STOP  RUN
*             消費税増税対応
         NOT  INVALID
*             消費税増税対応
              MOVE JYO-F04   TO   WK-ZEIRITU1
              MOVE JYO-F05   TO   WK-ZEIRITU2
              MOVE JYO-F06   TO   WK-ZEIKAITEI
              MOVE JYO-F07   TO   WK-ZEIRITU3
              MOVE JYO-F08   TO   WK-ZEIRITU4
*             消費税増税対応
     END-READ.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-JTOKCD    TO   DEN-F01.
     MOVE     PARA-JSOKCD    TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              DISPLAY NC"＃対象データ無し（Ｓ）！！" UPON CONS
              GO   TO   INIT-EXIT
     END-START.
*
     PERFORM   SHTDENLA-READ-SEC.
*
     IF  END-FLG = "END"
         DISPLAY NC"＃対象データ無し（Ｒ）！！" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　売上伝票ファイル読込
****************************************************************
 SHTDENLA-READ-SEC     SECTION.
*
     READ     SHTDENLA
              AT END    MOVE  "END"  TO  END-FLG
                        GO           TO  SHTDENLA-READ-EXIT
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*処理件数表示
     IF  RD-CNT(6:3)   =  "000"  OR  "500"
         DISPLAY  "RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*バッチ番号チェック
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-JTOKCD    =    DEN-F01 )
              IF   PARA-JSOKCD    NOT =     SPACE
                   IF   PARA-JSOKCD    =    DEN-F48
                        CONTINUE
                   ELSE
                        GO   TO   SHTDENLA-READ-SEC
                   END-IF
              END-IF
     ELSE
              MOVE     "END"      TO   END-FLG
              GO        TO       SHTDENLA-READ-EXIT
     END-IF.
*倉庫ＣＤ
     IF       PARA-JSOKCD    NOT =     SPACE
         IF   PARA-JSOKCD    =    DEN-F48
              CONTINUE
         ELSE
              GO        TO        SHTDENLA-READ-SEC
         END-IF
     ELSE
              CONTINUE
     END-IF.
*納品日
     IF       PARA-JNOUDT    NOT =     ZERO
         IF   PARA-JNOUDT    =    DEN-F112
              CONTINUE
         ELSE
              GO        TO        SHTDENLA-READ-SEC
         END-IF
     ELSE
              CONTINUE
     END-IF.
*    店舗コード
     IF     ( PARA-JTENST    <=   DEN-F07     ) AND
            ( DEN-F07        <=   PARA-JTENED )
              CONTINUE
     ELSE
              GO        TO        SHTDENLA-READ-SEC
     END-IF.
*    部門コード
     IF     ( PARA-JBUMST    <=   DEN-F12     ) AND
            ( DEN-F12        <=   PARA-JBUMED )
              CONTINUE
     ELSE
              GO        TO        SHTDENLA-READ-SEC
     END-IF.
*商品変換ＴＢＬ索引
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     PERFORM  SHOTBL1-READ-SEC.
     IF   SHOTBL1-INV-FLG  =  SPACE
          MOVE  TBL-F08      TO   WK-DEN-F49
     ELSE
          MOVE  DEN-F49      TO   WK-DEN-F49
     END-IF.
*    _番
*****DISPLAY "PARA-JTANAS = " PARA-JTANAS UPON CONS.
*    DISPLAY "PARA-JTANAE = " PARA-JTANAE UPON CONS.
*    DISPLAY "WK-DEN-F49  = " WK-DEN-F49  UPON CONS.
*****DISPLAY "**************************" UPON CONS.
     IF     ( PARA-JTANAS    <=   WK-DEN-F49  ) AND
            ( WK-DEN-F49     <=   PARA-JTANAE )
              CONTINUE
     ELSE
              GO        TO        SHTDENLA-READ-SEC
     END-IF.
*サブ商品変換ＴＢＬ索引
     MOVE     DEN-F01        TO   SUB-F01.
     MOVE     DEN-F25        TO   SUB-F02.
     PERFORM  SUBTBLL1-READ-SEC.
*商品名称マスタ索引
     MOVE     DEN-F1411      TO   MEI-F011.
     MOVE     DEN-F1412      TO   MEI-F012.
     PERFORM  MEIMS1-READ-SEC.
*
 SHTDENLA-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*出荷明細ワーク出力
     MOVE     SPACE          TO   SYK-REC.
     INITIALIZE                   SYK-REC.
*
     MOVE     DEN-F46        TO   SYK-F011.
     MOVE     DEN-F47        TO   SYK-F012.
     MOVE     DEN-F01        TO   SYK-F013.
     MOVE     DEN-F48        TO   SYK-F02.
     MOVE     DEN-F07        TO   SYK-F03.
     MOVE     DEN-F12        TO   SYK-F04.
     MOVE     WK-DEN-F49     TO   SYK-F05.
     MOVE     DEN-F1411      TO   SYK-F061.
     MOVE     DEN-F1412      TO   SYK-F062.
     MOVE     DEN-F25        TO   SYK-F07.
     MOVE     DEN-F1421      TO   SYK-F081.
     MOVE     DEN-F1422      TO   SYK-F082.
     IF  MEIMS1-INV-FLG = SPACE
         MOVE MEI-F021       TO   SYK-F083
         MOVE MEI-F022       TO   SYK-F084
     ELSE
         MOVE SPACE          TO   SYK-F083
         MOVE SPACE          TO   SYK-F084
     END-IF.
     MOVE     DEN-F50        TO   SYK-F09.
     MOVE     DEN-F15        TO   SYK-F10.
     MOVE     DEN-F172       TO   SYK-F11.
     MOVE     DEN-F173       TO   SYK-F12.
     MOVE     SPACE          TO   SYK-F13.
     MOVE     SPACE          TO   SYK-F14.
     MOVE     DEN-F112       TO   SYK-F15.
     MOVE     DEN-F02        TO   SYK-F16.
     MOVE     DEN-F27D       TO   SYK-F17.
*
     IF  TOK-F97  =  "0"
       IF  WK-ZEIKAITEI NOT =  ZERO
********消費税改定日以上の場合、新消費税率を使用
        IF  DEN-F112 >= WK-ZEIKAITEI
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU3
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU3
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU3 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYK-F18
        ELSE
************消費税改定日以下の場合、旧消費税率を使用
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU1
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYK-F18
        END-IF
       ELSE
************消費税改定日が指定されていない場合
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU1
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYK-F18
       END-IF
     END-IF.
*消費税税増税対応
*ＤＣＭ対応（ホーマック）
     IF  DEN-F01 = 880 OR 881 OR 882 OR 1427 OR 14272 OR 14273
         MOVE    DEN-REC             TO         HMC-REC
*********発注種別区分
*DS      MOVE    HMC-A161(1:1)       TO         SYK-F23(1:1)
*DS      MOVE    HMC-A161(2:1)       TO         SYK-F23(2:1)
*********形状区分
*DS      MOVE    HMC-A163            TO         SYK-F24
*********発注単位区分
*DS      MOVE    HMC-A162            TO         SYK-F25
*********発注単位区分＝Ｃのとき、形状区分をケースにする。
*DS      IF      HMC-A162  =  "C"
*DS              MOVE   "3"          TO         SYK-F24
*DS      END-IF
*#2020/04/06 NAV ST
         MOVE    HMC-M03             TO         SYK-F23
         MOVE    HMC-M16             TO         SYK-F24
         MOVE    HMC-M15             TO         SYK-F25
         IF      HMC-M15   =  "C"
                 MOVE   "3"          TO         SYK-F24
         END-IF
*#2020/04/06 NAV ED
     END-IF.
*ＤＣＭ対応（ホーマック以外）
     IF  DEN-F01 = 13938 OR 17137 OR 139381 OR 171371
         OR   1731  OR  1732  OR  7601  OR  7602  OR  100403
         OR   100404  OR  100427  OR  100428  OR  100441
         OR   100442
         MOVE DEN-REC        TO  KMA-REC
*********発注種別区分
*DS      MOVE KMA-A35(1:1)   TO  SYK-F23(1:1)
*DS      MOVE KMA-A35(2:1)   TO  SYK-F23(2:1)
*********形状区分
*DS      IF   DEN-F40  =  1
*DS           MOVE  KMA-A37  TO  SYK-F24
*DS      ELSE
*DS           MOVE  SPACE    TO  SYK-F24
*DS      END-IF
*********発注単位区分
*DS      MOVE KMA-A36        TO  SYK-F25
*#2020/04/06 NAV ST
         MOVE    KMA-M03             TO         SYK-F23
         MOVE    KMA-M16             TO         SYK-F24
         MOVE    KMA-M15             TO         SYK-F25
*********IF      KMA-M15   =  "C"
*                MOVE   "3"          TO         SYK-F24
*********END-IF
*#2020/04/06 NAV ED
     END-IF.
*エンチョーの場合
     IF  DEN-F01 = 24279
         MOVE DEN-REC        TO  ENC-REC
         MOVE ENC-A181       TO  SYK-F19
     END-IF.
*入数／ケース数／バラ数算出
*****入数算出
*****　サブ商品変換ＴＢＬが存在しない場合
*****DISPLAY "SUBTNLL1-INV-FLG = " SUBTBLL1-INV-FLG UPON CONS.
     IF  SUBTBLL1-INV-FLG = "INV"
*****　　商品名称Ｍが存在しない場合、入数に１セット
         IF  MEIMS1-INV-FLG = "INV"
             MOVE   1            TO   SYK-F20
         ELSE
*****　　　商品名称Ｍが存在し、入数が０または数値以外の場合、１
             IF MEI-F07  =  ZERO
             OR MEI-F07  NOT NUMERIC
                MOVE   1         TO   SYK-F20
             ELSE
*****　　　　　商品名称マスタの入数をセット
                MOVE   MEI-F07   TO   WK-IRISU
                MOVE   WK-IRISU  TO   SYK-F20
*****DISPLAY "MEI-F07  1 =  " MEI-F07     UPON CONS
*****DISPLAY "WK-IRISU 1 =  " WK-IRISU    UPON CONS
*****DISPLAY "SYK-F20  1 =  " SYK-F20     UPON CONS
             END-IF
         END-IF
     ELSE
         MOVE  SUB-F15           TO   WK-IRISU
         MOVE  WK-IRISU          TO   SYK-F20
*****DISPLAY "SUB-F15  1 =  " SUB-F15     UPON CONS
*****DISPLAY "WK-IRISU 1 =  " WK-IRISU    UPON CONS
*****DISPLAY "SYK-F20  1 =  " SYK-F20     UPON CONS
     END-IF.
*入数が取得出来ない場合
     IF  SYK-F20  =  ZERO
         MOVE  1                 TO   SYK-F20
     END-IF.
*ケース数／バラ数算出
     DIVIDE  SYK-F10  BY  SYK-F20
             GIVING   WK-SYO     REMAINDER   WK-AMARI.
     MOVE    WK-SYO              TO   SYK-F21.
     MOVE    WK-AMARI            TO   SYK-F22.
*
     WRITE  SYK-REC.
     ADD    1                    TO  WRT-CNT.
*
 MAIN-010.
*
     PERFORM  SHTDENLA-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 SHOTBL1-READ-SEC      SECTION.
*
     READ  SHOTBL1
           INVALID     MOVE  "INV"  TO  SHOTBL1-INV-FLG
           NOT INVALID MOVE  SPACE  TO  SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
****************************************************************
*　　サブ商品変換ＴＢＬ読込
****************************************************************
 SUBTBLL1-READ-SEC      SECTION.
*
     READ  SUBTBLL1
           INVALID     MOVE  "INV"  TO  SUBTBLL1-INV-FLG
           NOT INVALID MOVE  SPACE  TO  SUBTBLL1-INV-FLG
     END-READ.
*
 SUBTBLL1-READ-EXIT.
     EXIT.
****************************************************************
*　　商品名称マスタ読込
****************************************************************
 MEIMS1-READ-SEC       SECTION.
*
     READ  MEIMS1
           INVALID     MOVE  "INV"  TO  MEIMS1-INV-FLG
           NOT INVALID MOVE  SPACE  TO  MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENLA  SYKMEIF  SHOTBL1  SUBTBLL1  TOKMS2
               JYOKEN1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
