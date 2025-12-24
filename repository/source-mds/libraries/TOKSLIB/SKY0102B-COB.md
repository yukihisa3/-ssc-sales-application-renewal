# SKY0102B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY0102B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　_  サカタのタネ
*    業務名　　　　　：　売上システム　　　　　　　　　　　　　*
*    サブシステム名　：　日次処理　                            *
*    モジュール名　　：　オンラインデータ変換                  *
*    プログラムＩＤ　：　ＳＳＫＴ７２０　                      *
*    作成日　　　　　：　                                      *
*    作成者　　　　　：　NAV                                   *
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
****************************************************************
 IDENTIFICATION                      DIVISION.
 PROGRAM-ID.               OSKT720.
 ENVIRONMENT                         DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          FUJITSU.
 OBJECT-COMPUTER.          FUJITSU.
 SPECIAL-NAMES.
     YA          IS        PITCH-2
     YB          IS        PITCH-15
     YB-21       IS        BAIKAKU
     STATION     IS        STA.
*
 INPUT-OUTPUT              SECTION.
 FILE-CONTROL.
****************************
*ＩＮＰＵＴ　　共通ファイル*
****************************
     SELECT      HKYOTU    ASSIGN    TO        DA-01-S-HKYOTU
                           STATUS    IS        INK-ST.
****************************
*ＯＵＴＰＵＴ　共通ファイル*
****************************
     SELECT      HKYOTUW   ASSIGN    TO        DA-01-S-HKYOTUW
                           STATUS    IS        OUK-ST.
****************************
*ＩＮＰＵＴ　エラーファイル*
****************************
     SELECT      HIERRJNL  ASSIGN    TO        DA-01-S-HIERRJNL
                           STATUS    IS        INE-ST.
****************************
*ＯＵＴＰＵＴエラーファイル*
****************************
     SELECT      HOERRJNL  ASSIGN    TO        DA-01-S-HOERRJNL
                           STATUS    IS        OUE-ST.
****************************
*       取引先マスタ       *
****************************
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           STATUS    IS        TOK-ST.
****************************
*        店舗マスタ        *
****************************
     SELECT      HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TEN-F52   TEN-F011
                           STATUS    IS        TEN-ST.
****************************
*      商品変換マスタ      *
****************************
     SELECT      HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       STB-F01   STB-F02
                           STATUS    IS        STB-ST.
****************************
*      商品名称マスタ      *
****************************
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011  MEI-F0121
                                               MEI-F0122 MEI-F0123
                           STATUS    IS        MEI-ST.
****************************
*        プリンター　　    *
****************************
     SELECT      PRTF      ASSIGN    TO        LP-04.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************
*ＩＮＰＵＴ　　共通ファイル*
****************************
 FD  HKYOTU
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HKYOTU    OF        XFDLIB
     JOINING     INK       AS        PREFIX.
****************************
*ＯＵＴＰＵＴ　共通ファイル*
****************************
 FD  HKYOTUW
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HKYOTU    OF        XFDLIB
     JOINING     OUK       AS        PREFIX.
****************************
*ＩＮＰＵＴ　エラーファイル*
****************************
 FD  HIERRJNL
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HKYOTU    OF        XFDLIB
     JOINING     INE       AS        PREFIX.
****************************
*ＯＵＴＰＵＴエラーファイル*
****************************
 FD  HOERRJNL
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HKYOTU    OF        XFDLIB
     JOINING     OUE       AS        PREFIX.
****************************
*       取引先マスタ       *
****************************
 FD  HTOKMS
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************
*        店舗マスタ        *
****************************
 FD  HTENMS
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
****************************
*      商品変換マスタ      *
****************************
 FD  HSHOTBL
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HSHOTBL   OF        XFDLIB
     JOINING     STB       AS        PREFIX.
****************************
*      商品名称マスタ      *
****************************
 FD  HMEIMS
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
****************************
*        プリンター　　    *
****************************
 FD  PRTF
     LABEL       RECORD    IS        OMITTED
     LINAGE      IS        66.
 01  PRT-REC               PIC X(150).
****************************************************************
*            WORKING-STORAGE        SECTION                    *
****************************************************************
 WORKING-STORAGE           SECTION.
 01  IN-DATA               PIC X(01).
     COPY        HKYOTU    OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
**********************
*レコード退避テーブル*
**********************
 01  WK-RECTBL.
     03  WK-REC            PIC X(256)    OCCURS 99.
**********************
*レコードブレイク項目*
**********************
 01  WK-BRK.
     03  WK-TORCD          PIC 9(08).
     03  WK-DENNO          PIC 9(09).
***********************************************
*ファイル終了フラグ   "KEND"共通  "EEND"エラー*
***********************************************
 01  END-FLG               PIC X(04) VALUE     SPACE.
******************
*レコード退避添字*
******************
 01  IXA                   PIC 9(02) VALUE     ZERO.
 01  IXB                   PIC 9(02) VALUE     ZERO.
******************
*エラー発生フラグ*
******************
 01  ERR-FLG               PIC 9(01) VALUE     ZERO.
****************
*ページカウント*
****************
 01  WK-PAGE               PIC 9(03) VALUE     ZERO.
**************
*システム日付*
**************
 01  SYS-DATE.
     03  SYS-YY            PIC 9(02).
     03  SYS-MM            PIC 9(02).
     03  SYS-DD            PIC 9(02).
********************
*ファイルステータス*
********************
 01  WK-ST.
     03  INK-ST            PIC X(02).
     03  INK-ST1           PIC X(04).
     03  OUK-ST            PIC X(02).
     03  OUK-ST1           PIC X(04).
     03  INE-ST            PIC X(02).
     03  INE-ST1           PIC X(04).
     03  OUE-ST            PIC X(02).
     03  OUE-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  TEN-ST            PIC X(02).
     03  TEN-ST1           PIC X(04).
     03  STB-ST            PIC X(02).
     03  STB-ST1           PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
****************
*ファイルエラー*
****************
 01  FILE-ERR.
     03  INKERR            PIC N(15) VALUE
         NC"ＩＮＰＵＴ　　共通ファイル異常".
     03  OUKERR            PIC N(15) VALUE
         NC"ＯＵＴＰＵＴ　共通ファイル異常".
     03  INEERR            PIC N(15) VALUE
         NC"ＩＮＰＵＴ　エラーファイル異常".
     03  OUEERR            PIC N(15) VALUE
         NC"ＯＵＴＰＵＴエラーファイル異常".
     03  TOKERR            PIC N(15) VALUE
         NC"取引先マスタ異常".
     03  TENERR            PIC N(15) VALUE
         NC"店舗マスタ異常".
     03  STBERR            PIC N(15) VALUE
         NC"商品変換マスタ異常".
     03  MEIERR            PIC N(15) VALUE
         NC"商品名称マスタ異常".
**********************
*エラーリストレコード*
**********************
********
*見出し*
********
 01  LIST-M1.
     03  FILLER  CHARACTER TYPE      BAIKAKU.
         05  FILLER        PIC X(30) VALUE     SPACE.
         05  FILLER        PIC N(21) VALUE
           NC"【　オンラインデータ　変換エラーリスト　】".
         05  FILLER        PIC X(07) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"処理日".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LSYS-YY       PIC 9(02).
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-MM       PIC Z9.
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-DD       PIC Z9.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(01) VALUE
           NC"頁".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LPAGE         PIC ZZ9.
 01  LIST-M2.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"伝票_".
         05  FILLER        PIC X(04) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"取引先".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC N(02) VALUE
           NC"店舗".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"店舗名".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"取引先商品".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(05) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"商品名".
         05  FILLER        PIC X(17) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ売単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ売単価".
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"エラー内容".
********
*明細行*
********
 01  LIST-D.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  L-DEN         PIC 9(09).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TOR         PIC 9(08).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENCD       PIC 9(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENNM       PIC X(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHOCD       PIC X(13).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHONM       PIC X(18).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-ERR         PIC N(10).
 01  DUMMY                 PIC X(01) VALUE     SPACE.
****************************************************************
 PROCEDURE                           DIVISION.
****************************************************************
 DECLARATIVES.
 INK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HKYOTU.
     DISPLAY     INKERR    UPON      STA.
     DISPLAY     INK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 OUK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HKYOTUW.
     DISPLAY     OUKERR    UPON      STA.
     DISPLAY     OUK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 INE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HIERRJNL.
     DISPLAY     INEERR    UPON      STA.
     DISPLAY     INE-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 OUE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HOERRJNL.
     DISPLAY     OUEERR    UPON      STA.
     DISPLAY     OUE-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOKERR    UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TENERR    UPON      STA.
     DISPLAY     TEN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     STBERR    UPON      STA.
     DISPLAY     STB-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEIERR    UPON      STA.
     DISPLAY     MEI-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*
****************************************************************
 PROC-SEC                  SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = "EEND".
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
*
****************************************************************
*                         初期処理                             *
****************************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     HKYOTU
                           HIERRJNL
                           HTOKMS
                           HTENMS
                           HSHOTBL
                           HMEIMS
                 OUTPUT    HOERRJNL
                           HKYOTUW
                           PRTF.
     INITIALIZE  WK-RECTBL END-FLG   IXA       ERR-FLG.
     MOVE        SPACE     TO        LIST-D.
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
**************************
*共通Ｆ，ＥＲＲＦ初期読み*
**************************
     PERFORM     READ-SEC.
*******************************************************
*ＥＲＲＦがＡＴ ＥＮＤでなければ取引先ＣＤ，伝票_退避*
*******************************************************
     IF  (END-FLG NOT = "EEND")
         MOVE    DEN-F01   TO        WK-TORCD
         MOVE    DEN-F02   TO        WK-DENNO.
 INIT-EXIT.
     EXIT.
****************************************************************
*                                                              *
*                        ＭＡＩＮ処理                          *
*                                                              *
****************************************************************
 MAIN-SEC                  SECTION.
************************************
*取引先ＣＤ，伝票_ブレイクチェック*
************************************
     IF  (DEN-F01 NOT = WK-TORCD) OR
         (DEN-F02 NOT = WK-DENNO)
         MOVE    DEN-F01   TO        WK-TORCD
         MOVE    DEN-F02   TO        WK-DENNO
         PERFORM VARYING   IXB       FROM     1  BY  1
                 UNTIL     IXB       >        IXA
             PERFORM       DATA-WRT-SEC
         END-PERFORM
         MOVE    1         TO        IXA
         INITIALIZE        WK-RECTBL ERR-FLG
       ELSE
         ADD     1         TO        IXA.
**************************
*取引先マスタ存在チェック*
**************************
*****MOVE        SPACE     TO        LIST-D.
     MOVE        DEN-F01   TO        TOK-F01.
     READ        HTOKMS
       INVALID
         MOVE NC"取引先マスタ　未登録" TO      L-ERR
         PERFORM           ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    1         TO        ERR-FLG
       NOT INVALID
*自社得意先コード
         MOVE    TOK-F52   TO        DEN-F24
*端数区分，税端数区分
         MOVE    TOK-F86   TO        DEN-F279  DEN-F27A.
************************
*店舗マスタ存在チェック*
************************
*****MOVE        SPACE     TO        LIST-D.
     MOVE        DEN-F01   TO        TEN-F52.
     MOVE        DEN-F07   TO        TEN-F011.
     READ        HTENMS
       INVALID
         MOVE NC"店舗　マスタ　未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    1         TO        ERR-FLG
       NOT INVALID
*担当者コード
         MOVE    TEN-F11   TO        DEN-F06.
************************
*商品変換ＴＢＬチェック*
************************
*****MOVE        SPACE     TO        LIST-D.
     MOVE        DEN-F01   TO        STB-F01.
     MOVE        DEN-F25   TO        STB-F02.
     READ        HSHOTBL
       INVALID
         MOVE NC"商品変換ＴＢＬ未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    1         TO        ERR-FLG
       NOT INVALID
************************
*商品名称マスタチェック*
************************
*出荷場所，伝発場所
         MOVE    STB-F04   TO        DEN-F08   DEN-F09
*自社商品コード
         MOVE    STB-F031  TO        DEN-F1411 MEI-F011
*品単コード
         MOVE    STB-F032  TO        DEN-F1412 MEI-F012
         READ    HMEIMS
           INVALID
             MOVE NC"商品名称マスタ未登録" TO  L-ERR
             PERFORM   ERR-EDT-SEC
             PERFORM       ERR-WRT-SEC
             MOVE          1         TO        ERR-FLG
           NOT INVALID
*商品カナ名
             MOVE          MEI-F031  TO        DEN-F1421
             MOVE          MEI-F032  TO        DEN-F1422
*仕入単価
             MOVE          MEI-F041  TO        DEN-F171
         END-READ
******************
*単価ＥＱチェック*
******************
         IF  (DEN-F172 NOT = STB-F05) OR
             (DEN-F173 NOT = STB-F06)
             MOVE NC"単価が違います！　　" TO  L-ERR
             IF  (DEN-F172 NOT = STB-F05)
                 MOVE      STB-F05   TO        L-MGEN
             END-IF
             IF  (DEN-F173 NOT = STB-F06)
                 MOVE      STB-F06   TO        L-MURI
             END-IF
             PERFORM   ERR-EDT-SEC
             PERFORM       ERR-WRT-SEC
         END-IF
     END-READ.
********************
*ＷＯＲＫＲＥＣ転送*
********************
     MOVE        99        TO        DEN-F06.
     MOVE        DEN-REC   TO        WK-REC(IXA).
**************************
*共通Ｆ，ＥＲＲＦＲＥＡＤ*
**************************
     PERFORM     READ-SEC.
     IF  (END-FLG = "EEND")
         PERFORM VARYING   IXB       FROM     1  BY  1
                 UNTIL     IXB       >        IXA
             PERFORM       DATA-WRT-SEC
         END-PERFORM
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                  共通Ｆ，ＥＲＲＦＲＥＡＤ                    *
****************************************************************
 READ-SEC                  SECTION.
     IF  (END-FLG = SPACE)
         READ    HKYOTU
           AT END
                 MOVE     "KEND"     TO       END-FLG
           NOT AT END
                 MOVE      INK-REC   TO       DEN-REC
         END-READ
     END-IF.
     IF  (END-FLG NOT = SPACE)
         READ    HIERRJNL
           AT END
                 MOVE     "EEND"     TO       END-FLG
           NOT AT END
                 MOVE      INE-REC   TO       DEN-REC
         END-READ
     END-IF.
 READ-EXIT.
     EXIT.
****************************************************************
*                  共通Ｆ，ＥＲＲＦＷＲＩＴＥ                  *
****************************************************************
 DATA-WRT-SEC              SECTION.
     IF  (ERR-FLG = ZERO)
         MOVE    WK-REC(IXB)         TO        OUK-REC
         MOVE    SPACE               TO        OUK-F22
*粗利
         COMPUTE OUK-F20 = OUK-F181 - OUK-F15 * OUK-F171
*\\  92.06.12  START \\\
*        オンラインデータの単価は全て手入力扱いとする
         MOVE    "1"       TO         OUK-F16
*\\  92.06.12  END   \\\
*\\  94.05.24  START \\\
*        オンラインデータはすべて出荷日を　ゼロにする
         MOVE    ZERO      TO         OUK-F113
*\\  94.05.24  END   \\\
*
         WRITE   OUK-REC
         END-WRITE
     END-IF.
*****IF  (END-FLG NOT = ZERO)
     IF  (ERR-FLG NOT = ZERO)
         MOVE    WK-REC(IXB)         TO        OUE-REC
         WRITE   OUE-REC
         END-WRITE
     END-IF.
 DATA-WRT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト明細編集　　　                  *
****************************************************************
 ERR-EDT-SEC               SECTION.
     MOVE        DEN-F02   TO        L-DEN.
     MOVE        DEN-F01   TO        L-TOR.
     MOVE        DEN-F07   TO        L-TENCD.
     MOVE        DEN-F22   TO        L-TENNM.
     MOVE        DEN-F25   TO        L-SHOCD.
     MOVE        DEN-F1421 TO        L-SHONM.
     MOVE        DEN-F1422(1:3) TO   L-SHONM(16:3).
     MOVE        DEN-F172  TO        L-DGEN.
     MOVE        DEN-F173  TO        L-DURI.
 ERR-EDT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト出力　　　　　                  *
****************************************************************
 ERR-WRT-SEC               SECTION.
     IF  (WK-PAGE = ZERO) OR (LINAGE-COUNTER > 61)
         IF  (LINAGE-COUNTER > 61)
             MOVE          SPACE     TO        PRT-REC
             WRITE         PRT-REC   AFTER     PAGE
         END-IF
         ADD     1         TO        WK-PAGE
         MOVE    WK-PAGE   TO        LPAGE
         MOVE    SYS-YY    TO        LSYS-YY
         MOVE    SYS-MM    TO        LSYS-MM
         MOVE    SYS-DD    TO        LSYS-DD
         WRITE   PRT-REC   FROM      LIST-M1   AFTER     1
         WRITE   PRT-REC   FROM      LIST-M2   AFTER     2
         WRITE   PRT-REC   FROM      DUMMY     AFTER     1
     END-IF.
     WRITE       PRT-REC   FROM      LIST-D    AFTER     1.
     MOVE        SPACE     TO        LIST-D.
 ERR-WRT-EXIT.
     EXIT.
****************************************************************
*                        終了処理                              *
****************************************************************
 END-SEC                   SECTION.
     CLOSE                 HKYOTU
                           HIERRJNL
                           HTOKMS
                           HTENMS
                           HSHOTBL
                           HMEIMS
                           HOERRJNL
                           HKYOTUW
                           PRTF.
 END-EXIT.
     EXIT.

```
