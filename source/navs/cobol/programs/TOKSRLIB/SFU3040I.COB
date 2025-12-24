****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　                        *
*    業務名　　　　　　　：　社内振替　　　　　　　　　        *
*    モジュール名　　　　：　社内振替発注入力　                *
*    流用元　　　　　　　：　SIT0070I+SNJ0900I                 *
*    作成日／作成者　　　：　2017/01/24 INOUE                  *
*    更新日／更新者　　　：　2018/05/01 T.TAKAHASHI            *
*    更新内容　　　　　　：　伝票区分＝７０固定                *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SFU3040I.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2017/01/24.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          PG6000.
 OBJECT-COMPUTER.          PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*振替情報ファイル
     SELECT      SFRHEDL1  ASSIGN    TO        DA-01-VI-SFRHEDL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HED-F01
                                               HED-F02
                                               HED-F03
                                               HED-F04
                                               HED-F05
                                               HED-F06
                                               HED-F07
                                               HED-F08
                           FILE      STATUS    HED-ST.
*振替明細ファイル
     SELECT      SFRMEIL1  ASSIGN    TO        DA-01-VI-SFRMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MEI-F01
                                               MEI-F02
                                               MEI-F03
                                               MEI-F04
                                               MEI-F05
                                               MEI-F06
                                               MEI-F07
                                               MEI-F08
                                               MEI-F11
                                               MEI-F12
                           FILE      STATUS    MEI-ST.
*倉庫マスタ
     SELECT      ZSOKMS1   ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*担当者マスタ
     SELECT      TANMS1    ASSIGN    TO        DA-01-VI-TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*条件ファイル
     SELECT      JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
*シーズンマスタ
     SELECT      SEASONL1  ASSIGN    TO        DA-01-VI-SEASONL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SEA-F01
                           FILE      STATUS    SEA-ST.
*商品名称マスタ
     SELECT      MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       SHO-F011
                                               SHO-F0121
                                               SHO-F0122
                                               SHO-F0123
                           FILE      STATUS    SHO-ST.
*仕入先マスタ
     SELECT      ZSHIMS1   ASSIGN    TO        DA-01-VI-ZSHIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SHI-F01
                           FILE      STATUS    SHI-ST.
*画面ファイル*
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
                           SYMBOLIC  DESTINATION        "DSP"
                           DESTINATION-1       DSP-WS
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING  MODE    DSP-PRO
                           UNIT      CONTROL   DSP-UNIT
                           SELECTED  FUNCTION  DSP-FNC
                           FILE      STATUS    DSP-ST.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*振替情報ファイル
 FD  SFRHEDL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SFRHEDL1  OF        XFDLIB
     JOINING     HED       AS        PREFIX.
*振替明細ファイル
 FD  SFRMEIL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SFRMEIL1  OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS1   OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*担当者マスタ
 FD  TANMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        TANMS1    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*条件ファイル
 FD  JYOKEN1
     LABEL       RECORD    IS        STANDARD.
     COPY        JYOKEN1   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*シーズンマスタ
 FD  SEASONL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SEASONL1  OF        XFDLIB
     JOINING     SEA       AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        MEIMS1   OF        XFDLIB
     JOINING     SHO       AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSHIMS1   OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FFU30401  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  DSP-AREA.
     03  DSP-FMT           PIC X(08) VALUE     SPACE.
     03  DSP-GRP           PIC X(08) VALUE     SPACE.
     03  DSP-WS            PIC X(08) VALUE     SPACE.
     03  DSP-WSR           REDEFINES DSP-WS.
         05  DSP-WS1       PIC X(02).
         05  DSP-WS2       PIC 9(03).
         05  DSP-WS3       PIC X(01).
     03  DSP-PRO           PIC X(02) VALUE     SPACE.
     03  DSP-UNIT          PIC X(06) VALUE     SPACE.
     03  DSP-FNC           PIC X(04) VALUE     SPACE.
     03  DSP-ST            PIC X(02) VALUE     SPACE.
     03  DSP-ST1           PIC X(04) VALUE     SPACE.
*--------------------------------------------------------*
 01  MSG-AREA.
     03  MSG01           PIC N(20) VALUE
                         NC"登録されていません".
     03  MSG02           PIC N(20) VALUE
                         NC"すでに登録されています".
     03  MSG03           PIC N(20) VALUE
                         NC"シーズンを入力してください".
     03  MSG04           PIC N(20) VALUE
                         NC"伝票区分を入力してください".
     03  MSG05           PIC N(20) VALUE
                         NC"処理区分が違います".
     03  MSG06           PIC N(20) VALUE
                         NC"Ｙで入力してください".
     03  MSG07           PIC N(20) VALUE
                         NC"倉庫ＣＤを入力してください".
     03  MSG08           PIC N(20) VALUE
                         NC"年度（前後２年）を入力してください".
     03  MSG09           PIC N(20) VALUE
                         NC"倉庫Ｍが登録されていません".
     03  MSG10           PIC N(20) VALUE
                         NC"条件ファイル未登録の部門です".
     03  MSG11           PIC N(20) VALUE
                         NC"シーズンマスタ未登録です".
     03  MSG12           PIC N(20) VALUE
                         NC"商品コードを入力してください".
     03  MSG13           PIC N(20) VALUE
                         NC"商品名称マスタ未登録です".
     03  MSG14           PIC N(20) VALUE
                         NC"入荷予定日を入力してください".
     03  MSG15           PIC N(20) VALUE
                         NC"仕入先マスタ未登録です".
     03  MSG16           PIC N(20) VALUE
                         NC"完納区分は、空白または１です".
     03  MSG17           PIC N(20) VALUE
                         NC"日付が不正です".
     03  MSG18           PIC N(20) VALUE
                         NC"ＰＦキーが違います".
*--------------------------------------------------------*
     03  PMSG01            PIC N(20)
            VALUE  NC"_終了".
     03  PMSG02            PIC N(20)
            VALUE  NC"_取消".
     03  PMSG03            PIC N(20)
            VALUE  NC"_取消　_再入力".
     03  PMSG04            PIC N(20)
            VALUE  NC"_明細照会　_明細入力　_取消　_再入力".
     03  PMSG05            PIC N(20)
            VALUE  NC"_明細入力　_取消　_再入力".
     03  PMSG06            PIC N(20)
            VALUE  NC"_明細照会　_取消　_再入力".
*--------------------------------------------------------*
*日付／時刻
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*--------------------------------------------------------*
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  WK-SYUKKA         PIC X(08) VALUE     SPACE.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-SYORI          PIC 9(01) VALUE     ZERO.
     03  ZSOKMS1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  TANMS1-INV-FLG    PIC X(03) VALUE     SPACE.
     03  JYOKEN1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  SEASON1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  MEIMS1-INV-FLG    PIC X(03) VALUE     SPACE.
     03  ZSHIMS1-INV-FLG   PIC X(03) VALUE     SPACE.
*--------------------------------------------------------*
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  HED-ST            PIC X(02).
     03  MEI-ST            PIC X(02).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
     03  SEA-ST            PIC X(02).
     03  SHO-ST            PIC X(02).
     03  SHI-ST            PIC X(02).
*--------------------------------------------------------*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(11) VALUE
                                  NC"振替情報ファイル異常！".
     03  FILE-ERR3         PIC N(11) VALUE
                                  NC"振替明細ファイル異常！".
     03  FILE-ERR4         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
     03  FILE-ERR5         PIC N(10) VALUE
                                  NC"担当者マスタ異常！".
     03  FILE-ERR6         PIC N(10) VALUE
                                  NC"条件ファイル異常！".
     03  FILE-ERR7         PIC N(10) VALUE
                                  NC"シーズンマスタ異常！".
     03  FILE-ERR8         PIC N(10) VALUE
                                  NC"商品名称マスタ異常！".
     03  FILE-ERR9         PIC N(10) VALUE
                                  NC"仕入先マスタ異常！".
*--------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*レコード退避エリア
 01  DATA-TAIHI              PIC  X(400) VALUE SPACE.
*--------------------------------------------------------*
*画面退避エリア（更新後非消去項目）
 01  TAIHI-PGID              PIC  X(08) VALUE SPACE.
 01  TAIHI-FORMID            PIC  X(08) VALUE SPACE.
 01  TAIHI-SDATE             PIC  X(10) VALUE SPACE.
 01  TAIHI-STIME             PIC  X(08) VALUE SPACE.
 01  TAIHI-SYORI             PIC  X(01) VALUE SPACE.
 01  TAIHI-SYKNM             PIC  N(02) VALUE SPACE.
 01  TAIHI-MTANTO            PIC  X(07) VALUE SPACE.
 01  TAIHI-TANNM             PIC  N(05) VALUE SPACE.
 01  TAIHI-DENKCD            PIC  9(02) VALUE ZERO.
 01  TAIHI-DENKNM            PIC  N(04) VALUE SPACE.
 01  TAIHI-NENDO             PIC  9(04) VALUE ZERO.
 01  TAIHI-SEASON            PIC  X(02) VALUE SPACE.
 01  TAIHI-SEASNM            PIC  N(10) VALUE SPACE.
 01  TAIHI-SOKCD             PIC  X(02) VALUE SPACE.
 01  TAIHI-SOKNM             PIC  N(10) VALUE SPACE.
 01  TAIHI-SKTCD             PIC  X(08) VALUE SPACE.
 01  TAIHI-HINT1             PIC  X(05) VALUE SPACE.
 01  TAIHI-HINT2             PIC  X(02) VALUE SPACE.
 01  TAIHI-HINT3             PIC  X(01) VALUE SPACE.
*01  TAIHI-HACGK             PIC S9(09) VALUE ZERO.
*01  TAIHI-HACDAT            PIC  X(10) VALUE SPACE.
*01  TAIHI-NYKGK             PIC S9(09) VALUE ZERO.
*01  TAIHI-NYKDAT            PIC  X(10) VALUE SPACE.
*--------------------------------------------------------*
 01  SEQ                   PIC  9(02).
*--------------------------------------------------------*
*下位ＰＧ呼出パラメタ
*
*【社内振替明細入力】
 01  SFU3050I-PARA.
* 呼出区分
   03  SFU3050I-CALL         PIC  X(01) VALUE SPACE.
* 実行種別
   03  SFU3050I-SYUBETU      PIC  X(01) VALUE SPACE.
* 処理区分
   03  SFU3050I-SYORI        PIC  X(01) VALUE SPACE.
* 部門ＣＤ
   03  SFU3050I-BUMON        PIC  X(04) VALUE SPACE.
* 担当者ＣＤ
   03  SFU3050I-TANTOU       PIC  X(02) VALUE SPACE.
* 伝票区分
   03  SFU3050I-DENKU        PIC  X(02) VALUE SPACE.
* 年度
   03  SFU3050I-NENDO        PIC  9(04) VALUE ZERO.
* シーズン
   03  SFU3050I-SEASON       PIC  X(02) VALUE SPACE.
* 倉庫ＣＤ
   03  SFU3050I-SOUKO        PIC  X(02) VALUE SPACE.
* 商品ＣＤ
   03  SFU3050I-SHOHIN       PIC  X(08) VALUE SPACE.
* 品単１
   03  SFU3050I-HINTAN1      PIC  X(05) VALUE SPACE.
* 品単２
   03  SFU3050I-HINTAN2      PIC  X(02) VALUE SPACE.
* 品単３
   03  SFU3050I-HINTAN3      PIC  X(01) VALUE SPACE.
*
*【社内振替情報照会（明細）】
 01  SFU3070I-PARA.
* 伝票区分
   03  SFU3070I-DENKU        PIC  X(02) VALUE SPACE.
* 年度
   03  SFU3070I-NENDO        PIC  9(04) VALUE ZERO.
* シーズン
   03  SFU3070I-SEASON       PIC  X(02) VALUE SPACE.
* 倉庫ＣＤ
   03  SFU3070I-SOUKO        PIC  X(02) VALUE SPACE.
* 商品ＣＤ
   03  SFU3070I-SHOHIN       PIC  X(08) VALUE SPACE.
* 品単１
   03  SFU3070I-HINTAN1      PIC  X(05) VALUE SPACE.
* 品単２
   03  SFU3070I-HINTAN2      PIC  X(02) VALUE SPACE.
* 品単３
   03  SFU3070I-HINTAN3      PIC  X(01) VALUE SPACE.
* 仕入先ＣＤ
   03  SFU3070I-SHIIRE       PIC  9(08) VALUE ZERO.
* 発注数合計
   03  SFU3070I-HACKEI       PIC S9(09) VALUE ZERO.
* 入荷数合計
   03  SFU3070I-NYUKEI       PIC S9(09) VALUE ZERO.
*--------------------------------------------------------*
*呼出前モード退避
 01  BK-SYORI              PIC X(01) VALUE SPACE.
*初期完納区分退避
 01  BK-KANKBN             PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*前後２年度
 01  NENDO-BEFORE          PIC   9(04)  VALUE  ZERO.
 01  NENDO-AFTER           PIC   9(04)  VALUE  ZERO.
*--------------------------------------------------------*
*復帰制御用ＦＬＧ
 01  SFU3050I-SEL          PIC X(01) VALUE SPACE.
 01  SFU3050I-WRT          PIC X(01) VALUE SPACE.
 01  SFRHEDL1-HIT          PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*プログラムスイッチ
 01  PSW                   PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(02).
*--------------------------------------------------------*
******************************************************************
 PROCEDURE                 DIVISION  USING PARA-BUMONCD
                                           PARA-TANCD.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      CONS.
     DISPLAY     FILE-ERR1 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SFRHEDL1.
     DISPLAY     HED-ST    UPON      CONS.
     DISPLAY     FILE-ERR2 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SFRMEIL1.
     DISPLAY     MEI-ST    UPON      CONS.
     DISPLAY     FILE-ERR3 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS1.
     DISPLAY     SOK-ST    UPON      CONS.
     DISPLAY     FILE-ERR4 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           TANMS1.
     DISPLAY     TAN-ST    UPON      CONS.
     DISPLAY     FILE-ERR5 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           JYOKEN1.
     DISPLAY     JYO-ST    UPON      CONS.
     DISPLAY     FILE-ERR6 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SEA-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SEASONL1.
     DISPLAY     SEA-ST    UPON      CONS.
     DISPLAY     FILE-ERR7 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     SHO-ST    UPON      CONS.
     DISPLAY     FILE-ERR8 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS1.
     DISPLAY     SHI-ST    UPON      CONS.
     DISPLAY     FILE-ERR9 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM     END-SEC.
     STOP        RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     ZSOKMS1   JYOKEN1  TANMS1
                           SEASONL1  MEIMS1
                           ZSHIMS1.
     OPEN        I-O       SFRHEDL1  SFRMEIL1
                           DSPFILE.
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        HED-REC.
     INITIALIZE  HED-REC.
*----------------- 99/09/28追加 ---------------------*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE     "1"                 TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
*
     IF      PSW     =   "1"
             CONTINUE
     ELSE
             GO                TO        MAIN-020
     END-IF.
*
*初期画面表示
 MAIN-000.
     PERFORM     DSP-INIT-SEC.
*処理区分入力
 MAIN-010.
     MOVE        PMSG01     TO        PFMSG.
     MOVE       "PFMSG"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SYORI"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE     9      TO        END-FLG
           GO              TO        MAIN-EXIT
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*処理区分 CHK
     IF  (SYORI  =  1 OR 2 OR 3)
          MOVE    SPACE     TO        ERRMSG
          MOVE   "ERRMSG"   TO        DSP-GRP
          PERFORM DSP-WR-SEC
          MOVE   "M"        TO        EDIT-OPTION  OF  SYORI
          MOVE    SPACE     TO        EDIT-CURSOR  OF  SYORI
          IF      SYORI  =  1
                  MOVE   NC"登録"     TO            SYKNM
          END-IF
          IF      SYORI  =  2
                  MOVE   NC"修正"     TO            SYKNM
          END-IF
          IF      SYORI  =  3
                  MOVE   NC"削除"     TO            SYKNM
          END-IF
*         MOVE   "SYKNM"    TO        DSP-GRP
          MOVE   "MODE"     TO        DSP-GRP
*
          PERFORM DSP-WR-SEC
*#2018/05/01 NAV ST 伝票区分=70固定
          MOVE   70         TO        DENKCD
          MOVE   "DENKCD"   TO        DSP-GRP
          PERFORM DSP-WR-SEC
*#2018/05/01 NAV ED 伝票区分=70固定
       ELSE
         MOVE    MSG05     TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYORI
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-010
     END-IF.
 MAIN-020.
*  ＰＦキーガイダンス
     IF  (SYORI  =  2)
          MOVE   PMSG02      TO        PFMSG
     ELSE
          MOVE   PMSG02      TO        PFMSG
     END-IF.
     MOVE       "PFMSG"      TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*  キー情報入力
     MOVE       "HEAD01"     TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           MOVE  "1"         TO        PSW
           MOVE  " "         TO        SFU3050I-WRT
           GO                TO        MAIN-SEC
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG18     TO        ERRMSG
           MOVE   "ERRMSG"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO                TO        MAIN-020
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*入力値チェック
*  伝票区分
     IF ( DENKCD  =  ZERO )  OR  ( DENKCD  NOT NUMERIC )
         MOVE    MSG04       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"          TO        EDIT-OPTION  OF  DENKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  DENKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        1           TO        JYO-F01.
     MOVE        DENKCD      TO        JYO-F02.
     READ        JYOKEN1
       INVALID
         MOVE    MSG10       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"          TO        EDIT-OPTION  OF  DENKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    JYO-F03     TO        DENKNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  DENKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  年度
***  前後２年算出
     COMPUTE  NENDO-BEFORE =  HEN-DATE-YYYY - 2.
     COMPUTE  NENDO-AFTER  =  HEN-DATE-YYYY + 2.
***  未入力
     IF ( NENDO   NOT NUMERIC )  OR
        ( NENDO   =   ZERO    )
         MOVE    MSG08     TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  NENDO
         MOVE   "C"        TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  NENDO
         MOVE    SPACE     TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
***  許容年
     IF ( NENDO  <   NENDO-BEFORE ) OR
        ( NENDO  >   NENDO-AFTER  )
         MOVE    MSG08     TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  NENDO
         MOVE   "C"        TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  NENDO
         MOVE    SPACE     TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*  シーズン
     IF  SEASON  =  SPACE
         MOVE    MSG03       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SEASNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SEASON
         MOVE   "C"          TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SEASON
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        SEASON      TO        SEA-F01.
     READ        SEASONL1
       INVALID
         MOVE    MSG11       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SEASNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SEASON
         MOVE   "C"          TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    SEA-F02     TO        SEASNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  SEASON
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  倉庫
     IF  SOKCD   =  SPACE
         MOVE    MSG07       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SOKNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SOKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SOKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        SOKCD       TO        SOK-F01.
     READ        ZSOKMS1
       INVALID
         MOVE    MSG09       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SOKNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SOKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    SOK-F02     TO        SOKNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  SOKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  商品ＣＤ・品単ＣＤ
     IF  SKTCD   =  SPACE
         MOVE    MSG12       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SYONM1
         MOVE    SPACE       TO        SYONM2
         MOVE   "R"          TO        EDIT-OPTION  OF  SKTCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SKTCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        SKTCD       TO        SHO-F011.
     MOVE        HINT1       TO        SHO-F0121.
     MOVE        HINT2       TO        SHO-F0122.
     MOVE        HINT3       TO        SHO-F0123.
     READ        MEIMS1
       INVALID
         MOVE    MSG13       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SHO-F021
         MOVE    SPACE       TO        SHO-F022
         MOVE   "R"          TO        EDIT-OPTION  OF  SKTCD
                                       EDIT-OPTION  OF  HINT1
                                       EDIT-OPTION  OF  HINT2
                                       EDIT-OPTION  OF  HINT3
         MOVE   "C"          TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
*        MOVE    SHO-F021    TO        SYONM1
*        MOVE    SHO-F022    TO        SYONM2
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  SKTCD
                                       EDIT-OPTION  OF  HINT1
                                       EDIT-OPTION  OF  HINT2
                                       EDIT-OPTION  OF  HINT3
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*
 MAIN-028.
*ファイル存在チェック
     MOVE    DENKCD            TO        HED-F01.
     MOVE    NENDO             TO        HED-F02.
     MOVE    SEASON            TO        HED-F03.
     MOVE    SOKCD             TO        HED-F04.
     MOVE    SKTCD             TO        HED-F05.
     MOVE    HINT1             TO        HED-F06.
     MOVE    HINT2             TO        HED-F07.
     MOVE    HINT3             TO        HED-F08.
     MOVE    " "               TO        SFRHEDL1-HIT.
     READ    SFRHEDL1
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        ERRMSG
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "M"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-029
         ELSE
             MOVE    MSG01     TO        ERRMSG
             MOVE    1         TO        INV-SW
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE    SPACE       TO        SYONM1
             MOVE    SPACE       TO        SYONM2
             MOVE   "R"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "C"        TO        EDIT-CURSOR  OF  DENKCD
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         MOVE    "H"           TO        SFRHEDL1-HIT
         IF  (SYORI  =  1) AND ( SFU3050I-SEL = " " )
             MOVE    MSG02     TO        ERRMSG
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "C"        TO        EDIT-CURSOR  OF  DENKCD
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
*
*明細初期内容表示
 MAIN-029.
     IF  ( SYORI  =  1 ) AND ( SFRHEDL1-HIT = " " )
*                　　　　　　　　　　商品名
         MOVE    SHO-F021    TO        SYONM1
         MOVE    SHO-F022    TO        SYONM2
*                　　　　　　　　　　仕入先
         MOVE    SHO-F05     TO        SIRCD  SHI-F01
         READ    ZSHIMS1
           INVALID
             MOVE    ALL NC"＊"   TO   SIRNM
           NOT INVALID
             MOVE    SHI-F02      TO   SIRNM
         END-READ
*                　　　　　　　　　　ＪＡＮＣＤ
         MOVE    SHO-F06     TO        JANCD
     ELSE
*                　　　　　　　　　　商品名
         MOVE    HED-F09     TO        SYONM1
         MOVE    HED-F10     TO        SYONM2
*                　　　　　　　　　　入荷予定日
         MOVE    HED-F14     TO        YOTDAT
*                　　　　　　　　　　仕入先
         MOVE    HED-F12     TO        SIRCD  SHI-F01
         READ    ZSHIMS1
           INVALID
             MOVE    ALL NC"＊"   TO   SIRNM
           NOT INVALID
             MOVE    SHI-F02      TO   SIRNM
         END-READ
*                　　　　　　　　　　棚番
         MOVE    HED-F11     TO        TANBAN
*                　　　　　　　　　　ＪＡＮＣＤ
         MOVE    HED-F13     TO        JANCD
*                　　　　　　　　　　ＭＳＧ
         MOVE    HED-F23     TO        HMESSG
*                　　　　　　　　　　完納情報
         MOVE    HED-F24     TO        KANKBN  BK-KANKBN
         IF      HED-F24     =         "1"
                 MOVE    HED-F26  TO   KANTAN(1:4) TAN-F01
                 MOVE    "-"      TO   KANTAN(5:1)
                 MOVE    HED-F27  TO   KANTAN(6:2) TAN-F02
                 READ    TANMS1
                   INVALID
                     MOVE    ALL NC"＊"   TO   KTANNM
                   NOT INVALID
                     MOVE    TAN-F03      TO   KTANNM
                 END-READ
                 MOVE    HED-F25(1:4)     TO   KANDAT(1:4)
                 MOVE    "/"              TO   KANDAT(5:1)
                 MOVE    HED-F25(5:2)     TO   KANDAT(6:2)
                 MOVE    "/"              TO   KANDAT(8:1)
                 MOVE    HED-F25(7:2)     TO   KANDAT(9:2)
         ELSE
                 MOVE    SPACE            TO   KANTAN
                                               KTANNM
                                               KANDAT
         END-IF
*                　　　　　　　　　　発注数合計
*T
*        DISPLAY "HED-F15=" HED-F15 UPON CONS
*T
         MOVE    HED-F15          TO   HACGK
*                　　　　　　　　　　最終発注日
         IF    ( HED-F18  NOT = ZERO  ) AND
               ( HED-F18  IS  NUMERIC )
                 MOVE    HED-F18(1:4)     TO   HACDAT(1:4)
                 MOVE    "/"              TO   HACDAT(5:1)
                 MOVE    HED-F18(5:2)     TO   HACDAT(6:2)
                 MOVE    "/"              TO   HACDAT(8:1)
                 MOVE    HED-F18(7:2)     TO   HACDAT(9:2)
         ELSE
                 MOVE    SPACE            TO   HACDAT
         END-IF
*                　　　　　　　　　　入荷数合計
         MOVE    HED-F19          TO   NYKGK
*                　　　　　　　　　　最終入荷日
         IF    ( HED-F22  NOT = ZERO  ) AND
               ( HED-F22  IS  NUMERIC )
                 MOVE    HED-F22(1:4)     TO   NYKDAT(1:4)
                 MOVE    "/"              TO   NYKDAT(5:1)
                 MOVE    HED-F22(5:2)     TO   NYKDAT(6:2)
                 MOVE    "/"              TO   NYKDAT(8:1)
                 MOVE    HED-F22(7:2)     TO   NYKDAT(9:2)
         ELSE
                 MOVE    SPACE            TO   NYKDAT
         END-IF
     END-IF.
*
     MOVE       "MEISAI"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
     IF  (SYORI  =  3)
         GO                  TO        MAIN-040
     END-IF.
*
 MAIN-030.
     MOVE        PMSG03      TO        PFMSG.
     MOVE       "PFMSG"      TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
     IF          SFU3050I-SEL    =   "Y"
                 MOVE     " "      TO  SFU3050I-SEL
                 GO                TO  MAIN-040
     END-IF.
*
*項目 入力
     MOVE       "MEISAI"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE  "1"      TO  PSW
                    MOVE  " "      TO  SFU3050I-WRT
                    GO             TO  MAIN-SEC
         WHEN      "F009"
                    GO             TO  MAIN-020
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG18     TO   ERRMSG
                             MOVE   "ERRMSG"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
*
     MOVE     SPACE          TO   ERRMSG.
     MOVE     "ERRMSG"       TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
     MOVE       "MEISAI"   TO        DSP-GRP.
     PERFORM  DSP-WR-SEC.
*項目入力チェック
*　入荷予定日
     IF ( YOTDAT  NOT NUMERIC ) OR
        ( YOTDAT   =  ZERO    )
         MOVE     "R"        TO   EDIT-OPTION  OF  YOTDAT
         MOVE     "C"        TO   EDIT-CURSOR  OF  YOTDAT
         MOVE      MSG14     TO   ERRMSG
         MOVE     "ERRMSG"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "MEISAI"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
***  論理チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     YOTDAT         TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              MOVE     "R"        TO   EDIT-OPTION  OF  YOTDAT
              MOVE     "C"        TO   EDIT-CURSOR  OF  YOTDAT
              MOVE      MSG17     TO   ERRMSG
              MOVE     "ERRMSG"     TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              MOVE     "MEISAI"   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              GO   TO   MAIN-030
         END-IF
*
         MOVE     "M"        TO   EDIT-OPTION  OF  YOTDAT
         MOVE      SPACE     TO   EDIT-CURSOR  OF  YOTDAT
         MOVE     "MEISAI"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*
*　仕入先ＣＤ
     IF ( SIRCD   IS     NUMERIC ) AND
        ( SIRCD   NOT =  ZERO    )
         MOVE    SIRCD       TO        SHI-F01
         READ    ZSHIMS1
           INVALID
             MOVE    ALL NC"＊"  TO   SIRNM
             MOVE     "R"        TO   EDIT-OPTION  OF  SIRCD
             MOVE     "C"        TO   EDIT-CURSOR  OF  SIRCD
             MOVE      MSG15     TO   ERRMSG
             MOVE     "ERRMSG"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "MEISAI"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
           NOT INVALID
             MOVE      SHI-F02   TO   SIRNM
             MOVE     "M"        TO   EDIT-OPTION  OF  SIRCD
             MOVE      SPACE     TO   EDIT-CURSOR  OF  SIRCD
             MOVE     "MEISAI"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-READ
     ELSE
         MOVE      ZERO      TO   SIRCD
         MOVE      SPACE     TO   SIRNM
     END-IF.
*
*　完納区分
     IF ( KANKBN  NOT = " "   ) AND
        ( KANKBN  NOT = "1"   )
         MOVE     "R"        TO   EDIT-OPTION  OF  KANKBN
         MOVE     "C"        TO   EDIT-CURSOR  OF  KANKBN
         MOVE      MSG16     TO   ERRMSG
         MOVE     "ERRMSG"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "MEISAI"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
     IF ( KANKBN   =    " "  )
         MOVE      SPACE     TO   KANTAN
                                  KTANNM
                                  KANDAT
     END-IF.
     IF ( KANKBN   =    "1"  )
          IF ( BK-KANKBN   =    "1"  )
               CONTINUE
          ELSE
               MOVE  PARA-BUMONCD TO KANTAN(1:4)
               MOVE  "-"          TO KANTAN(5:1)
               MOVE  PARA-TANCD   TO KANTAN(6:2)
               MOVE  TAN-F03              TO   KTANNM
               MOVE  HEN-DATE             TO   KANDAT
     END-IF.
     MOVE     "M"        TO   EDIT-OPTION  OF  KANKBN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KANKBN.
     MOVE     "MEISAI"   TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
*
 MAIN-040.
*確認入力
     MOVE     "Y"            TO   ANS.
     MOVE     "ANS"          TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
     IF        SYORI   =   1
               MOVE      PMSG05        TO   PFMSG
     END-IF.
     IF        SYORI   =   2
               MOVE      PMSG04        TO   PFMSG
     END-IF.
     IF        SYORI   =   3
               MOVE      PMSG06        TO   PFMSG
     END-IF.
     MOVE     "PFMSG"        TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "ANS"          TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN      "F001"
*          社内振替情報照会（明細）ＣＡＬＬ
           IF  (SYORI  = 2 OR 3)
                MOVE     DENKCD   TO        SFU3070I-DENKU
                MOVE     NENDO    TO        SFU3070I-NENDO
                MOVE     SEASON   TO        SFU3070I-SEASON
                MOVE     SOKCD    TO        SFU3070I-SOUKO
                MOVE     SKTCD    TO        SFU3070I-SHOHIN
                MOVE     HINT1    TO        SFU3070I-HINTAN1
                MOVE     HINT2    TO        SFU3070I-HINTAN2
                MOVE     HINT3    TO        SFU3070I-HINTAN3
                MOVE     SIRCD    TO        SFU3070I-SHIIRE
                MOVE     HACGK    TO        SFU3070I-HACKEI
                MOVE     NYKGK    TO        SFU3070I-NYUKEI
                CALL    "SFU3070I"  USING   SFU3070I-DENKU
                                            SFU3070I-NENDO
                                            SFU3070I-SEASON
                                            SFU3070I-SOUKO
                                            SFU3070I-SHOHIN
                                            SFU3070I-HINTAN1
                                            SFU3070I-HINTAN2
                                            SFU3070I-HINTAN3
                                            SFU3070I-SHIIRE
                                            SFU3070I-HACKEI
                                            SFU3070I-NYUKEI
*   　          元画面に戻る
                MOVE   "CL"            TO      DSP-PRO
                MOVE   "SCREEN"        TO      DSP-GRP
                MOVE    TAIHI-PGID     TO      PGID
                MOVE    TAIHI-FORMID   TO      FORMID
                MOVE    TAIHI-SDATE    TO      SDATE
                MOVE    TAIHI-STIME    TO      STIME
                MOVE    TAIHI-MTANTO   TO      MTANTO
                MOVE   "FFU30401"      TO      DSP-FMT
                WRITE   FFU30401
                MOVE    SPACE          TO      DSP-PRO
                GO                     TO      MAIN-040
           ELSE
                MOVE    MSG18     TO   ERRMSG
                MOVE   "ERRMSG"   TO   DSP-GRP
                PERFORM DSP-WR-SEC
                GO   TO   MAIN-040
           END-IF
         WHEN      "F002"
*          社内振替明細入力　ＣＡＬＬ
           IF  (SYORI  = 1 OR 2)
                MOVE     "Y"           TO   SFU3050I-SEL
                PERFORM  SFRHEDL1-SET-SEC
                PERFORM  SFRHEDL1-UPDT-SEC
*???            MOVE     "Y"           TO   SFU3050I-WRT
                MOVE     "P"           TO   SFU3050I-CALL
                MOVE     "1"           TO   SFU3050I-SYUBETU
                MOVE     SYORI         TO   SFU3050I-SYORI
                MOVE     PARA-BUMONCD  TO   SFU3050I-BUMON
                MOVE     PARA-TANCD    TO   SFU3050I-TANTOU
                MOVE     DENKCD        TO   SFU3050I-DENKU
                MOVE     NENDO         TO   SFU3050I-NENDO
                MOVE     SEASON        TO   SFU3050I-SEASON
                MOVE     SOKCD         TO   SFU3050I-SOUKO
                MOVE     SKTCD         TO   SFU3050I-SHOHIN
                MOVE     HINT1         TO   SFU3050I-HINTAN1
                MOVE     HINT2         TO   SFU3050I-HINTAN2
                MOVE     HINT3         TO   SFU3050I-HINTAN3
                CALL    "SFU3050I"  USING   SFU3050I-CALL
                                            SFU3050I-SYUBETU
                                            SFU3050I-SYORI
                                            SFU3050I-BUMON
                                            SFU3050I-TANTOU
                                            SFU3050I-DENKU
                                            SFU3050I-NENDO
                                            SFU3050I-SEASON
                                            SFU3050I-SOUKO
                                            SFU3050I-SHOHIN
                                            SFU3050I-HINTAN1
                                            SFU3050I-HINTAN2
                                            SFU3050I-HINTAN3
*   　          元画面に戻る
                MOVE   "CL"            TO      DSP-PRO
                MOVE   "SCREEN"        TO      DSP-GRP
                MOVE    TAIHI-PGID     TO      PGID
                MOVE    TAIHI-FORMID   TO      FORMID
                MOVE    TAIHI-SDATE    TO      SDATE
                MOVE    TAIHI-STIME    TO      STIME
                MOVE    TAIHI-MTANTO   TO      MTANTO
                MOVE   "FFU30401"      TO      DSP-FMT
                WRITE   FFU30401
                MOVE    SPACE          TO      DSP-PRO
                GO                     TO      MAIN-028
           ELSE
                MOVE    MSG18     TO   ERRMSG
                MOVE   "ERRMSG"   TO   DSP-GRP
                PERFORM DSP-WR-SEC
                GO   TO   MAIN-040
           END-IF
         WHEN      "F004"
           MOVE     SPACE    TO        ANS
           MOVE    "1"       TO        PSW
           MOVE  " "         TO        SFU3050I-WRT
           GO                TO        MAIN-SEC
         WHEN      "F009"
           IF  (SYORI  = 1 OR 2)
                             GO        TO   MAIN-030
           ELSE
                             GO        TO   MAIN-020
           END-IF
         WHEN      "E000"
           CONTINUE
         WHEN      OTHER
                             MOVE    MSG18     TO   ERRMSG
                             MOVE   "ERRMSG"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO   TO   MAIN-040
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*
     IF  (ANS  NOT =  "Y")
           MOVE    MSG06       TO   ERRMSG
           MOVE   "ERRMSG"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE       TO   ERRMSG
           MOVE   "ERRMSG"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
     END-IF.
*
     IF  (SYORI  =  3)
           GO          TO   MAIN-080.
*画面から転送
     PERFORM  SFRHEDL1-SET-SEC.
 MAIN-080.
     PERFORM  SFRHEDL1-UPDT-SEC.
*
     MOVE    SYORI          TO      TAIHI-SYORI.
     MOVE    SYKNM          TO      TAIHI-SYKNM.
     MOVE    DENKCD         TO      TAIHI-DENKCD.
     MOVE    DENKNM         TO      TAIHI-DENKNM.
     MOVE    NENDO          TO      TAIHI-NENDO.
     MOVE    SEASON         TO      TAIHI-SEASON.
     MOVE    SEASNM         TO      TAIHI-SEASNM.
     MOVE    SOKCD          TO      TAIHI-SOKCD.
     MOVE    SOKNM          TO      TAIHI-SOKNM.
     MOVE    SKTCD          TO      TAIHI-SKTCD.
     MOVE    HINT1          TO      TAIHI-HINT1.
     MOVE    HINT2          TO      TAIHI-HINT2.
     MOVE    HINT3          TO      TAIHI-HINT3.
     MOVE    SPACE          TO      FFU30401.
     MOVE    TAIHI-PGID     TO      PGID.
     MOVE    TAIHI-FORMID   TO      FORMID.
     MOVE    TAIHI-SDATE    TO      SDATE.
     MOVE    TAIHI-STIME    TO      STIME.
     MOVE    TAIHI-SYORI    TO      SYORI.
     MOVE    TAIHI-SYKNM    TO      SYKNM.
     MOVE    TAIHI-MTANTO   TO      MTANTO.
     MOVE    TAIHI-TANNM    TO      TANNM.
     MOVE    TAIHI-DENKCD   TO      DENKCD.
     MOVE    TAIHI-DENKNM   TO      DENKNM.
     MOVE    TAIHI-NENDO    TO      NENDO.
     MOVE    TAIHI-SEASON   TO      SEASON.
     MOVE    TAIHI-SEASNM   TO      SEASNM.
     MOVE    TAIHI-SOKCD    TO      SOKCD.
     MOVE    TAIHI-SOKNM    TO      SOKNM.
     MOVE    TAIHI-SKTCD    TO      SKTCD.
     MOVE    TAIHI-HINT1    TO      HINT1.
     MOVE    TAIHI-HINT2    TO      HINT2.
     MOVE    TAIHI-HINT3    TO      HINT3.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
*
     GO             TO      MAIN-020.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             SFRHEDL1  SFRMEIL1  ZSOKMS1  TANMS1
                       JYOKEN1   SEASONL1  MEIMS1   ZSHIMS1
                       DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"               TO  DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面    ＷＲＩＴＥ                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FFU30401.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FFU30401.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FFU30401"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     MOVE   "SFU3040I"       TO   PGID.
     MOVE   "FFU30401"       TO   FORMID.
     MOVE    PARA-BUMONCD    TO   MTANTO(1:4)  TAN-F01.
     MOVE   "-"              TO   MTANTO(5:1).
     MOVE    PARA-TANCD      TO   MTANTO(6:2)  TAN-F02.
     MOVE    SPACE           TO   BK-KANKBN.
     READ    TANMS1
        INVALID
             MOVE    ALL NC"＊"   TO   TAN-F03
     END-READ
     MOVE    TAN-F03        TO      TANNM.
     MOVE    PGID           TO      TAIHI-PGID.
     MOVE    FORMID         TO      TAIHI-FORMID.
     MOVE    SDATE          TO      TAIHI-SDATE.
     MOVE    STIME          TO      TAIHI-STIME.
     MOVE    MTANTO         TO      TAIHI-MTANTO.
     MOVE    TANNM          TO      TAIHI-TANNM.
*
     WRITE   FFU30401.
     INITIALIZE              FFU30401.
*
 DSP-INIT-EXIT.
     EXIT.
*
****************************************************************
*            画面よりレコードに転送                            *
****************************************************************
 SFRHEDL1-SET-SEC           SECTION.
     MOVE     DENKCD         TO        HED-F01.
     MOVE     NENDO          TO        HED-F02.
     MOVE     SEASON         TO        HED-F03.
     MOVE     SOKCD          TO        HED-F04.
     MOVE     SKTCD          TO        HED-F05.
     MOVE     HINT1          TO        HED-F06.
     MOVE     HINT2          TO        HED-F07.
     MOVE     HINT3          TO        HED-F08.
     MOVE     SYONM1         TO        HED-F09.
     MOVE     SYONM2         TO        HED-F10.
     MOVE     TANBAN         TO        HED-F11.
     IF    (  SIRCD   IS  NUMERIC  )
        OR (  SIRCD   NOT =  ZERO  )
              MOVE    SIRCD  TO        HED-F12
     END-IF.
     MOVE     JANCD          TO        HED-F13.
     MOVE     YOTDAT         TO        HED-F14.
     IF    (  SYORI        =    "1"  ) AND
           (  SFU3050I-WRT =    " "  )
              MOVE           ZERO      TO        HED-F15
              MOVE           SPACE     TO        HED-F16
              MOVE           SPACE     TO        HED-F17
              MOVE           ZERO      TO        HED-F18
              MOVE           ZERO      TO        HED-F19
              MOVE           SPACE     TO        HED-F20
              MOVE           SPACE     TO        HED-F21
              MOVE           ZERO      TO        HED-F22
     END-IF.
     MOVE     HMESSG         TO        HED-F23.
     MOVE     KANKBN         TO        HED-F24.
     IF       KANKBN    =    "1"
         IF   BK-KANKBN   =    " "
              MOVE  SYS-DATE      TO   HED-F25
              MOVE  KANTAN(1:4)   TO   HED-F26
              MOVE  KANTAN(6:2)   TO   HED-F27
         END-IF
     ELSE
              MOVE  ZERO          TO   HED-F25
              MOVE  SPACE         TO   HED-F26
                                       HED-F27
     END-IF.
     MOVE     SPACE          TO        HED-F91.
     IF       SYORI     =    "1"
              MOVE  PARA-BUMONCD  TO   HED-F92
              MOVE  PARA-TANCD    TO   HED-F93
              MOVE  SYS-DATE      TO   HED-F94
              MOVE  WK-TIME(1:6)  TO   HED-F95
              MOVE  PARA-BUMONCD  TO   HED-F96
              MOVE  PARA-TANCD    TO   HED-F97
              MOVE  SYS-DATE      TO   HED-F98
              MOVE  WK-TIME(1:6)  TO   HED-F99
     END-IF.
     IF       SYORI     =    "2"
              MOVE  PARA-BUMONCD  TO   HED-F96
              MOVE  PARA-TANCD    TO   HED-F97
              MOVE  SYS-DATE      TO   HED-F98
              MOVE  WK-TIME(1:6)  TO   HED-F99
     END-IF.
*
 SFRHEDL1-SET-EXIT.
     EXIT.
****************************************************************
*  ファイル更新処理
****************************************************************
 SFRHEDL1-UPDT-SEC      SECTION.
*
*T↓
*    DISPLAY "SYORI         =" SYORI UPON CONS.
*    DISPLAY "WK-SYORI      =" WK-SYORI UPON CONS.
*    DISPLAY "BK-SYORI      =" BK-SYORI UPON CONS.
*    DISPLAY "TAIHI-SYORI   =" TAIHI-SYORI UPON CONS.
*    DISPLAY "SFU3050I-SYORI=" SFU3050I-SYORI UPON CONS.
*    DISPLAY "SFU3050I-WRT  =" SFU3050I-WRT   UPON CONS.
*T↑
     EVALUATE   SYORI
         WHEN     1
*             MOVE     SYS-DATE   TO  HED-F98
              MOVE     HED-REC    TO  DATA-TAIHI
              IF       SFU3050I-WRT = " "
                       WRITE    HED-REC
                       END-WRITE
                       MOVE     "Y"           TO   SFU3050I-WRT
              ELSE
                       REWRITE  HED-REC
                       END-REWRITE
                       MOVE SPACE TO  SFU3050I-WRT
              END-IF
         WHEN     2
              MOVE     HED-REC    TO  DATA-TAIHI
              REWRITE  HED-REC
              END-REWRITE
              MOVE     SPACE      TO  SFU3050I-WRT
         WHEN     3
              PERFORM  SFRMEIL1-DEL-SEC
              MOVE     HED-REC    TO  DATA-TAIHI
              DELETE   SFRHEDL1
              END-DELETE
              MOVE     SPACE      TO  SFU3050I-WRT
     END-EVALUATE.
*
 SFRHEDL1-UPDT-EXIT.
     EXIT.
****************************************************************
*  振替明細ファイル削除
****************************************************************
 SFRMEIL1-DEL-SEC       SECTION.
*
 SFRMEIL1-DEL-01.
     MOVE     HED-F01        TO        MEI-F01.
     MOVE     HED-F02        TO        MEI-F02.
     MOVE     HED-F03        TO        MEI-F03.
     MOVE     HED-F04        TO        MEI-F04.
     MOVE     HED-F05        TO        MEI-F05.
     MOVE     HED-F06        TO        MEI-F06.
     MOVE     HED-F07        TO        MEI-F07.
     MOVE     HED-F08        TO        MEI-F08.
     MOVE     SPACE          TO        MEI-F11.
     MOVE     ZERO           TO        MEI-F12.
*
 SFRMEIL1-DEL-02.
     START    SFRMEIL1  KEY  IS  >=    MEI-F01
                                       MEI-F02
                                       MEI-F03
                                       MEI-F04
                                       MEI-F05
                                       MEI-F06
                                       MEI-F07
                                       MEI-F08
                                       MEI-F11
                                       MEI-F12
         INVALID
              GO             TO        SFRMEIL1-DEL-EXIT
     END-START.
*
 SFRMEIL1-DEL-03.
     READ     SFRMEIL1       NEXT AT END
              GO             TO        SFRMEIL1-DEL-EXIT
     END-READ.
*
     IF    (  MEI-F01 =  HED-F01  )  AND
           (  MEI-F02 =  HED-F02  )  AND
           (  MEI-F03 =  HED-F03  )  AND
           (  MEI-F04 =  HED-F04  )  AND
           (  MEI-F05 =  HED-F05  )  AND
           (  MEI-F06 =  HED-F06  )  AND
           (  MEI-F07 =  HED-F07  )  AND
           (  MEI-F08 =  HED-F08  )
              CONTINUE
     ELSE
              GO             TO        SFRMEIL1-DEL-EXIT
     END-IF.
*
 SFRMEIL1-DEL-04.
     DELETE   SFRMEIL1.
*
 SFRMEIL1-DEL-05.
     GO       TO             SFRMEIL1-DEL-03.
*
 SFRMEIL1-DEL-EXIT.
     EXIT.
