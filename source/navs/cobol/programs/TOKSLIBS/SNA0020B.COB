****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　小売苗業務　連携　　　　　　　　　*
*    業務名　　　　　　　：　小売連携発注データ抽出（ＯＮＬ）　*
*    モジュール名　　　　：　小売連携発注データ抽出（ＯＮＬ）  *
*    作成日／更新日　　　：　2011/10/27                        *
*    作成者／更新者　　　：　ＮＡＶ飯田　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*                            ※連携ナンバーは８桁までのカウント*
*                              とする                          *
*　　更新日／更新者　　　：　2012/07/12                        *
*　　更新日／更新者　　　：　NAV INOUE                         *
*    修正概要　　　　　　：                                    *
*      売上伝票データの小売連携区分はノーチェックにし、        *
*      倉庫マスタの小売連携区分をチェックする。                *
*　　更新日／更新者　　　：　2012/09/04 NAV TAKAHASHI          *
*    修正概要　　　　　　：                                    *
*      取引先マスタより、計上部門コードを受取り、計上部門コード*
*      を連携データにセットする。　　　　　　　　　　　　　　　*
*　　　計上部門→Ｆ０２、所属部門→Ｆ３２にセット　　　　　　　*
*　　更新日／更新者　　　：　2012/09/21 NAV TAKAHASHI          *
*    修正概要　　　　　　：                                    *
*      出荷数量＝０のデータは送信対象としない。　　　　　　　　*
*　　更新日／更新者　　　：　2012/10/10 NAV TAKAHASHI          *
*    修正概要　　　　　　：                                    *
*      出荷場所転送項目を変更する。　　　　　　　　　　　　　　*
*    変更日／更新日　：2016/10/20                              *
*    変更内容　　　　：                                        *
*      小売連携商品のみを対象とする                            *
*      ２０１２／０７／１２対応分の戻し作業                    *
*    変更日／更新日　：2017/03/03                              *
*    変更内容　　　　：                                        *
*      ナフコの時、自社伝票番号の先頭３桁に店舗ＣＤをセットに  *
*      変更する。　　　　　　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SNA0020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/10/27.
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
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
***2011.10.06(DEN-F07,DEN-F112)
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-ST.
*小売連携発注データ
     SELECT   EXXXXXXA  ASSIGN    TO        DA-01-S-EXXXXXXA
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   XXX-ST.
*最新連携_ファイル
     SELECT   NARRNOF   ASSIGN    TO        DA-01-VI-NARRNOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   RNO-F01
                        FILE      STATUS    IS   RNO-ST.
*連携_管理テーブル
     SELECT   NARKANF   ASSIGN    TO        DA-01-VI-NARKANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KAN-F01
                        FILE      STATUS    IS   KAN-ST.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    IS   MEI-ST.
*↓2012/07/12
*倉庫マスタ
     SELECT  ZSOKMS     ASSIGN    TO        ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    IS   SOK-ST.
*↑2012/07/12
*↓2012/09/04
*取引先マスタ
     SELECT  HTOKMS     ASSIGN    TO        TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    IS   TOK-ST.
*↑2012/09/04
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    小売連携発注データ
******************************************************************
 FD  EXXXXXXA           BLOCK     CONTAINS   1   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     EXXXXXXA  OF        XFDLIB
              JOINING   XXX       PREFIX.
******************************************************************
*    連携_管理テーブル
******************************************************************
 FD  NARRNOF            LABEL RECORD   IS   STANDARD.
     COPY     NARRNOF   OF        XFDLIB
              JOINING   RNO       PREFIX.
*
******************************************************************
*    連携_管理テーブル
******************************************************************
 FD  NARKANF            LABEL RECORD   IS   STANDARD.
     COPY     NARKANF   OF        XFDLIB
              JOINING   KAN       PREFIX.
******************************************************************
*    商品名称マスタ（最新の連携区分を取得のため）
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*↓2012/07/12
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*↑2012/07/12
*↓2012/09/04
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*↑2012/09/04
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  NARRNOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NARKANF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*↓2012/07/12
 01  ZSOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
*↑2012/07/12
*↓2012/09/04
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
*↑2012/09/04
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  DEN-ST        PIC  X(02).
     03  XXX-ST        PIC  X(02).
     03  RNO-ST        PIC  X(02).
     03  KAN-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
*↓2012/07/12
     03  SOK-ST        PIC  X(02).
*↑2012/07/12
*↓2012/09/04
     03  TOK-ST        PIC  X(02).
*↑2012/09/04
*連携_採番
 01  WK-RENKEI-NO          PIC  X(09).
 01  WK-RENKEI-NO-R        REDEFINES  WK-RENKEI-NO.
     03  WK-RENKEI-NO-H    PIC  9(09).
*↓2012.4.18
 01  WK-RENKEI-NO-R2       REDEFINES  WK-RENKEI-NO.
     03  WK-RENKEI-NO-H1   PIC  9(01).
     03  WK-RENKEI-NO-H8   PIC  9(08).
*↑2012.4.18
*連携_採番
 01  WK-BACHI-NO           PIC  X(20).
 01  WK-BACHI-NO-R         REDEFINES  WK-BACHI-NO.
     03  WK-BACHI-NO-1     PIC  9(08).
     03  WK-BACHI-NO-2     PIC  9(04).
     03  WK-BACHI-NO-3     PIC  9(08).
*↓2012.09.04
*部門コード退避
 01  WK-BUMON.
     03  WK-BUMON-1        PIC  9(04).
     03  WK-BUMON-2        PIC  9(08).
     03  WK-BUMON-3        PIC  9(04).
     03  WK-BUMON-4        PIC  X(04).
*↑2012.09.03
*店舗変換
 01  WK-TENPO-CD           PIC  9(06).
 01  WK-TENPO-CD-R         REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-CD-H     PIC  X(06).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

 01  MSG-AREA.
     03  MSG-WAKU           PIC  N(21)  VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-01             PIC  N(21)  VALUE
         NC"＊　以下の連携Ｎｏでデータを抽出します　＊".
     03  MSG-02.
         05  FILLER         PIC  X(04)  VALUE "＊".
         05  FILLER         PIC  X(12)  VALUE SPACE.
         05  MSG-02-RENNO   PIC  X(09).
         05  FILLER         PIC  X(17)  VALUE SPACE.
         05  FILLER         PIC  X(04)  VALUE "＊".

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  RNO-ERR           PIC  N(20)  VALUE
         NC"最新連携Ｎｏファイルエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TXX-ERR           PIC  N(20)  VALUE
         NC"小売連携発注データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
*↓2012/07/12
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".
*↑2012/07/12
*↓2012/09/04
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
*↑2012/09/04
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SNA0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SNA0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " KANRI  = ".
         05  OUT-CNT1       PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメータ
 01  PARA-IN-IP             PIC   X(15).
*   パラメータＡ
 01  PARA-IN-BUMONTANCD     PIC   X(06).
 01  PARA-IN-ONLINE-TEGAKI  PIC   X(01).
 01  PARA-IN-BACHI-NO       PIC   X(20).
 01  PARA-IN-BASYO          PIC   X(02).
 01  PARA-IN-NOUDT          PIC   9(08).
 01  PARA-IN-JITU-NOUDT     PIC   9(08).
 01  PARA-IN-TORICD         PIC   9(08).
 01  PARA-IN-DENPYO1-ST     PIC   9(09).
 01  PARA-IN-DENPYO1-ED     PIC   9(09).
 01  PARA-IN-DENPYO2-ST     PIC   9(09).
 01  PARA-IN-DENPYO2-ED     PIC   9(09).
 01  PARA-IN-DENPYO3-ST     PIC   9(09).
 01  PARA-IN-DENPYO3-ED     PIC   9(09).
 01  PARA-IN-DENPYO4-ST     PIC   9(09).
 01  PARA-IN-DENPYO4-ED     PIC   9(09).
 01  PARA-IN-DENPYO5-ST     PIC   9(09).
 01  PARA-IN-DENPYO5-ED     PIC   9(09).
 01  PARA-IN-JYOTAI         PIC   X(09).
 01  PARA-IN-RENKEI-NO      PIC   X(09).
* 出力
*   パラメータＢ
 01  PARA-OUT-BUMON-TANCD   PIC   X(06).
 01  PARA-OUT-ONLINE-TEGAKI PIC   X(01).
 01  PARA-OUT-RENKEI-NO-ST  PIC   X(09).
 01  PARA-OUT-RENKEI-NO-ED  PIC   X(09).
 01  PARA-OUT-IP            PIC   X(15).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-IP
                            PARA-IN-BUMONTANCD
                            PARA-IN-ONLINE-TEGAKI
                            PARA-IN-BACHI-NO
                            PARA-IN-BASYO
                            PARA-IN-NOUDT
                            PARA-IN-JITU-NOUDT
                            PARA-IN-TORICD
                            PARA-IN-DENPYO1-ST
                            PARA-IN-DENPYO1-ED
                            PARA-IN-DENPYO2-ST
                            PARA-IN-DENPYO2-ED
                            PARA-IN-DENPYO3-ST
                            PARA-IN-DENPYO3-ED
                            PARA-IN-DENPYO4-ST
                            PARA-IN-DENPYO4-ED
                            PARA-IN-DENPYO5-ST
                            PARA-IN-DENPYO5-ED
                            PARA-IN-JYOTAI
                            PARA-IN-RENKEI-NO
                            PARA-OUT-BUMON-TANCD
                            PARA-OUT-ONLINE-TEGAKI
                            PARA-OUT-RENKEI-NO-ST
                            PARA-OUT-RENKEI-NO-ED
                            PARA-OUT-IP.
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RNO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRNOF.
     DISPLAY     RNO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RNO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TXX-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EXXXXXXA.
     DISPLAY     TXX-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     XXX-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↓2012/07/12
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2012/07/12
*↓2012/09/04
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2012/09/04
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
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SHTDENF.
     OPEN     I-O       NARRNOF.
     OPEN     I-O       NARKANF.
     OPEN     INPUT     HMEIMS.
     OPEN     OUTPUT    EXXXXXXA.
*↓2012/07/12
     OPEN     INPUT     ZSOKMS.
*↑2012/07/12
*↓2012/09/04
     OPEN     INPUT     HTOKMS.
*↑2012/09/04
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    パラメタ　バッチ_変換
     MOVE     PARA-IN-BACHI-NO   TO   WK-BACHI-NO.
     DISPLAY "#ﾊﾞｯﾁNO. = " WK-BACHI-NO-1 "-"
                           WK-BACHI-NO-2 "-"
                           WK-BACHI-NO-3 " #"  UPON CONS.
*↓2012.09.04 部門ワーク初期化／取引先マスタ読込（部門取得）
     INITIALIZE                   WK-BUMON.
*取引先マスタ読込（部門コード取得）
     MOVE     WK-BACHI-NO-3      TO   TOK-F01.
     PERFORM  RD-HTOKMS-SEC.
     IF  HTOKMS-INV-FLG = "INV"
         MOVE 4000      TO        PROGRAM-STATUS
         DISPLAY NC"＃＃取引先マスタ未登録！！＃＃" UPON CONS
         STOP  RUN
     ELSE
         MOVE TOK-F75   TO        WK-BUMON-1
         MOVE TOK-F76   TO        WK-BUMON-2
         MOVE TOK-F77   TO        WK-BUMON-3
         IF  WK-BUMON-2 NOT = ZERO
         AND SYS-DATEW  >= WK-BUMON-2
             MOVE WK-BUMON-3      TO    WK-BUMON-4
             IF   WK-BUMON-4  =  ZERO
                  MOVE WK-BUMON-1 TO    WK-BUMON-4
             END-IF
         ELSE
             MOVE WK-BUMON-1      TO    WK-BUMON-4
         END-IF
     END-IF.
     DISPLAY "KEIJYOU-BUMON = " WK-BUMON-4  UPON  CONS.
*↑2012.09.04 部門ワーク初期化／取引先マスタ読込（部門取得）
*売上伝票ファイルスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     WK-BACHI-NO-1  TO   DEN-F46.
     MOVE     WK-BACHI-NO-2  TO   DEN-F47.
     MOVE     WK-BACHI-NO-3  TO   DEN-F01.
     MOVE     PARA-IN-BASYO  TO   DEN-F48.
     START    SHTDENF  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
***2011.10.06(DEN-F07,DEN-F112)
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     ELSE
**************連携_採番
              PERFORM NARRNOF-READ-SEC
     END-IF.
*
 INIT-010.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*レコード初期化
     MOVE     SPACE               TO   XXX-REC.
     INITIALIZE                        XXX-REC.
*連携_
     MOVE     WK-RENKEI-NO        TO   XXX-F01.
     MOVE     WK-RENKEI-NO        TO   DEN-F33.
*部門コード
*↓2012.09.04転送項目を変更する。（計上部門）
*****MOVE     PARA-IN-BUMONTANCD(1:4)
*****                             TO   XXX-F02.
     MOVE     WK-BUMON-4          TO   XXX-F02.
*↑2012.09.04転送項目を変更する。（計上部門）
*担当者コード
     MOVE     PARA-IN-BUMONTANCD(5:2)
                                  TO   XXX-F03.
*取引先コード
*****MOVE     PARA-IN-TORICD      TO   XXX-F04.
     MOVE     DEN-F24             TO   XXX-F04.
*自社得意先コード
*****MOVE     DEN-F24             TO   XXX-F05.
     MOVE     PARA-IN-TORICD      TO   XXX-F05.
*バッチ_
     MOVE     WK-BACHI-NO-1       TO   XXX-F061.
     MOVE     WK-BACHI-NO-2       TO   XXX-F062.
     MOVE     WK-BACHI-NO-3       TO   XXX-F063.
*注文日
     MOVE     DEN-F111            TO   HIDUKE-HENKAN.
     MOVE     HIDUKE-HENKAN       TO   XXX-F07.
*納品日
     MOVE     PARA-IN-NOUDT       TO   XXX-F08
*実納品日
     MOVE     PARA-IN-JITU-NOUDT  TO   XXX-F09.
*店舗コード
     MOVE     DEN-F07             TO   WK-TENPO-CD.
     MOVE     WK-TENPO-CD-H       TO   XXX-F10.
*伝票区分
     MOVE     DEN-F051            TO   XXX-F11.
*自社伝票番号
     MOVE     DEN-F02             TO   XXX-F12.
*↓2017/03/03 NAV ST ナフコ時対応
     IF       DEN-F01  =  137607
              MOVE  DEN-F07(3:3)  TO   XXX-F12(1:3)
     END-IF.
*↑2017/03/03 NAV ED ナフコ時対応
*相手伝票番号
     MOVE     DEN-F23             TO   XXX-F13.
*送信元端末ＩＰ
     MOVE     PARA-IN-IP          TO   XXX-F14.
*入力区分
     MOVE     PARA-IN-ONLINE-TEGAKI  TO   XXX-F15.
*ルート
     MOVE     DEN-F42             TO   XXX-F16.
*伝票行番号
     MOVE     DEN-F03             TO   XXX-F17.
*出荷場所コード
***I MOVE     PARA-IN-BASYO       TO   XXX-F18.
*↓2012/10/10 変更開始
*****MOVE     DEN-F48             TO   XXX-F18.
     MOVE     DEN-F08             TO   XXX-F18.
*↑2012/10/10 変更終了
*自社商品コード
     MOVE     DEN-F1411           TO   XXX-F19.
*品単コード
     MOVE     DEN-F1412           TO   XXX-F20.
*相手商品コード
     MOVE     DEN-F25             TO   XXX-F21.
*数量
     MOVE     DEN-F15             TO   XXX-F22.
*原価単価
     MOVE     DEN-F172            TO   XXX-F23.
*売価単価
     MOVE     DEN-F173            TO   XXX-F24.
*原価金額
     MOVE     DEN-F181            TO   XXX-F25.
*売価金額
     MOVE     DEN-F182            TO   XXX-F26.
*明細備考
     MOVE     DEN-F22             TO   XXX-F27.
*伝票備考
     MOVE     SPACE               TO   XXX-F28.
*↓2012/07/12
*小売連携区分
     MOVE     DEN-F32             TO   XXX-F29.
*商品名(カナ)1
     MOVE     DEN-F1421           TO   XXX-F30.
*商品名(カナ)2
     MOVE     DEN-F1422           TO   XXX-F31.
*↑2012/07/12
*↓2012/09/04
*所属部門コード
     MOVE     PARA-IN-BUMONTANCD(1:4)
                                  TO   XXX-F32.
*↑2012/09/04
*処理区分
     MOVE     "1"                 TO   XXX-F97.
*依頼日付
     MOVE     SYS-DATEW           TO   XXX-F98.
*依頼時刻
     MOVE     SYS-HHMNSS          TO   XXX-F99.
*小売連携発注データ出力
     WRITE    XXX-REC.
*売上伝票ファイルへ連携_更新
     REWRITE  DEN-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF  WRT-CNT1 > ZERO
         PERFORM RENKEI-NO-SEC
         MOVE PARA-IN-BUMONTANCD        TO PARA-OUT-BUMON-TANCD
         MOVE PARA-IN-ONLINE-TEGAKI     TO PARA-OUT-ONLINE-TEGAKI
         MOVE WK-RENKEI-NO              TO PARA-OUT-RENKEI-NO-ST
         MOVE WK-RENKEI-NO              TO PARA-OUT-RENKEI-NO-ED
         MOVE PARA-IN-IP                TO PARA-OUT-IP
     END-IF.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT1  TO      OUT-CNT.
     MOVE      WRT-CNT2  TO      OUT-CNT1.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-OUT1  UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     SHTDENF  NARRNOF  NARKANF  EXXXXXXA  HMEIMS.
*↓2012/07/12
     CLOSE     ZSOKMS.
*↑2012/07/12
*↓2012/09/04
     CLOSE     HTOKMS.
*↑2012/09/04
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     READ     SHTDENF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SHTDENF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
 DEN-010.
*指定バッチ_チェック
     IF     ( WK-BACHI-NO-1  =  DEN-F46 ) AND
            ( WK-BACHI-NO-2  =  DEN-F47 ) AND
            ( WK-BACHI-NO-3  =  DEN-F01 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENF-READ-EXIT
     END-IF.
 DEN-020.
*出荷場所チェック
*    条件指定で対象と判別されている倉庫の場合は、
*    倉庫マスタの小売連携区分をチェックする必要なし。
     IF       PARA-IN-BASYO  NOT =  SPACE
              IF  PARA-IN-BASYO  NOT =  DEN-F48
                  GO              TO   SHTDENF-READ-SEC
              END-IF
     END-IF.
 DEN-030.
*↓2012/07/12
*出荷場所チェック(無指定)
*    条件指定で出荷場所が無指定の場合は、
*    倉庫マスタの小売連携区分をチェックする必要あり。
     IF       PARA-IN-BASYO      =  SPACE
              MOVE   DEN-F48      TO   SOK-F01
              PERFORM RD-ZSOKMS-SEC
              IF    ZSOKMS-INV-FLG = "INV"
                    GO TO  SHTDENF-READ-SEC
              END-IF
              IF    SOK-F12   NOT = "1"
                    GO TO  SHTDENF-READ-SEC
              END-IF
     END-IF.
 DEN-040.
*↑2012/07/12
*納品日チェック
     MOVE     DEN-F112            TO   WK-DEN-F112.
     IF       PARA-IN-NOUDT  NOT =  WK-DEN-F112
              GO                  TO   SHTDENF-READ-SEC
     END-IF.
 DEN-050.
*↓2012/07/12
*連携区分
     PERFORM  HMEIMS-READ-SEC.
* ↓2016/10/20 ST 小売連携区分のみ転送に変更対応
     IF       HMEIMS-INV-FLG  =  "INV"
******DISPLAY "DEN-051 = " DEN-F1411 " - " DEN-F1412 " - " MEI-F10
              GO                  TO   SHTDENF-READ-SEC
     ELSE  *>商品名称マスタの小売連携区分をチェック
              IF   MEI-F10 NOT = "1"
******DISPLAY "DEN-052 = " DEN-F1411 " - " DEN-F1412 " - " MEI-F10
                   GO             TO   SHTDENF-READ-SEC
              ELSE
                   MOVE  MEI-F10  TO   DEN-F32
              END-IF
     END-IF.
*****IF       HMEIMS-INV-FLG  =  "INV"
*****         CONTINUE
*****ELSE
*****         MOVE  MEI-F10  TO   DEN-F32
*****END-IF.
* ↑2016/10/20 ED 小売連携区分のみ転送に変更対応
*↑2012/07/12
 DEN-060.
*オンライン区分
     IF       DEN-F274 = 1    *> オンライン
              CONTINUE
     ELSE *> 手書き
              GO                  TO   SHTDENF-READ-SEC
     END-IF.

 DEN-070.
*小売連携管理Ｎｏ
     IF  DEN-F33 = SPACE *> 未設定のものを対象とする。
         CONTINUE
     ELSE
         GO TO  SHTDENF-READ-SEC
     END-IF.
 DEN-070.
*↓ 2012/09/21 ST
*出荷数量が０以下の明細は抽出対象としない。
     IF  DEN-F15  >  ZERO
         CONTINUE
     ELSE
         GO TO  SHTDENF-READ-SEC
     END-IF.
*↑ 2012/09/21 ED

 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　連携_採番
****************************************************************
 NARRNOF-READ-SEC          SECTION.
*
     MOVE     1             TO            RNO-F01.
     READ     NARRNOF
              INVALID       MOVE  "INV"   TO  NARRNOF-INV-FLG
              NOT INVALID   MOVE  SPACE   TO  NARRNOF-INV-FLG
     END-READ.
*存在した場合→連携_カウントアップ／しない場合→レコード作成
     IF  NARRNOF-INV-FLG = "INV"
         INITIALIZE  RNO-REC
         MOVE  1            TO  RNO-F01
         MOVE  ZERO         TO  RNO-F02
     END-IF.

*連携_数値に変換
     MOVE     RNO-F02             TO   WK-RENKEI-NO.
*カウントアップ
*↓2012.4.18
*****ADD      1                   TO   WK-RENKEI-NO-H.
     ADD      1                   TO   WK-RENKEI-NO-H8.
*↑2012.4.18
*
*カウントアップし、連携_が０になった場合
*↓2012.4.18
*****IF  WK-RENKEI-NO-H  = ZERO
     IF  WK-RENKEI-NO-H8 = ZERO
*↑2012.4.18
         MOVE 1                   TO   WK-RENKEI-NO-H
     END-IF.
*カウント連携_を更新
     MOVE     WK-RENKEI-NO        TO   RNO-F02.
*更新日／更新時間セット
     MOVE     SYS-DATEW           TO   RNO-F98.
     MOVE     SYS-HHMNSS          TO   RNO-F99.
*更新
     IF  NARRNOF-INV-FLG = "INV"
         WRITE  RNO-REC
     ELSE
         REWRITE  RNO-REC
     END-IF.
*
* 採番した連携Ｎｏをコンソールに表示。
     DISPLAY  MSG-WAKU  UPON CONS.
     DISPLAY  MSG-01    UPON CONS.
     MOVE  WK-RENKEI-NO     TO  MSG-02-RENNO.
     DISPLAY  MSG-02    UPON CONS.
     DISPLAY  MSG-WAKU  UPON CONS.

 NARRNOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     MOVE     DEN-F1411     TO         MEI-F011.
     MOVE     DEN-F1412     TO         MEI-F012.
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*↓2012/07/12
****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     READ  ZSOKMS
       INVALID
         MOVE  "INV"             TO  ZSOKMS-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  ZSOKMS-INV-FLG
     END-READ.
 RD-ZSOKMS-EXIT.
     EXIT.
*↑2012/07/12
*↓2012/09/04
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     READ  HTOKMS
       INVALID
         MOVE  "INV"             TO  HTOKMS-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  HTOKMS-INV-FLG
     END-READ.
 RD-HTOKMS-EXIT.
     EXIT.
*↑2012/09/04
****************************************************************
*　　　　　　　連携_管理テーブル出力
****************************************************************
 RENKEI-NO-SEC         SECTION.
*
     MOVE     "RENKEI-NO-SEC"    TO        S-NAME.
*
     MOVE      WK-RENKEI-NO      TO        KAN-F01.
     READ      NARKANF
               INVALID      MOVE "INV" TO  NARKANF-INV-FLG
               NOT  INVALID MOVE SPACE TO  NARKANF-INV-FLG
     END-READ.
*レコード初期化
     MOVE      SPACE             TO        KAN-REC.
     INITIALIZE                            KAN-REC.
*連携_
     MOVE      WK-RENKEI-NO      TO        KAN-F01.
*部門コード
     MOVE     PARA-IN-BUMONTANCD(1:4)
                                           TO        KAN-F02.
*担当者コード
     MOVE     PARA-IN-BUMONTANCD(5:2)
                                           TO        KAN-F03.
*状態ＦＬＧ
     MOVE      "1"                         TO        KAN-F04.
*前回状態ＦＬＧ
     MOVE      SPACE                       TO        KAN-F05.
*データ件数
     MOVE      WRT-CNT1                    TO        KAN-F06.
*入力区分
     MOVE      PARA-IN-ONLINE-TEGAKI       TO        KAN-F071.
*バッチ_
     MOVE      WK-BACHI-NO-1               TO        KAN-F0721.
     MOVE      WK-BACHI-NO-2               TO        KAN-F0722.
     MOVE      WK-BACHI-NO-3               TO        KAN-F0723.
*場所コード
     MOVE      PARA-IN-BASYO               TO        KAN-F073.
*納品日
     MOVE      PARA-IN-NOUDT               TO        KAN-F074.
*実納品日
     MOVE      PARA-IN-JITU-NOUDT          TO        KAN-F075.
*取引先コード
     MOVE      WK-BACHI-NO-3               TO        KAN-F076.
*指定伝票_１（開始）
     MOVE      PARA-IN-DENPYO1-ST          TO        KAN-F077.
*　　　　　　（終了）
     MOVE      PARA-IN-DENPYO1-ED          TO        KAN-F078.
*指定伝票_２（開始）
     MOVE      PARA-IN-DENPYO2-ST          TO        KAN-F079.
*　　　　　　（終了）
     MOVE      PARA-IN-DENPYO2-ED          TO        KAN-F07A.
*指定伝票_３（開始）
     MOVE      PARA-IN-DENPYO3-ST          TO        KAN-F07B.
*　　　　　　（終了）
     MOVE      PARA-IN-DENPYO3-ED          TO        KAN-F07C.
*指定伝票_４（開始）
     MOVE      PARA-IN-DENPYO4-ST          TO        KAN-F07D.
*　　　　　　（終了）
     MOVE      PARA-IN-DENPYO4-ED          TO        KAN-F07E.
*指定伝票_５（開始）
     MOVE      PARA-IN-DENPYO5-ST          TO        KAN-F07F.
*　　　　　　（終了）
     MOVE      PARA-IN-DENPYO5-ED          TO        KAN-F07G.
*依頼日付
     MOVE      SYS-DATEW                   TO        KAN-F96.
*依頼時刻
     MOVE      SYS-HHMNSS                  TO        KAN-F97.
*返答日付
     MOVE      ZERO                        TO        KAN-F98.
*返答時刻
     MOVE      ZERO                        TO        KAN-F99.
*連携_管理テーブル出力
     IF  NARKANF-INV-FLG = "INV"
         WRITE     KAN-REC
     ELSE
         REWRITE   KAN-REC
     END-IF.
*件数カウント
     ADD       1                           TO        WRT-CNT2.
*
 RENKEI-NO-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
