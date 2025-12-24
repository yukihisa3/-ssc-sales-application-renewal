# SNA0620B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SNA0620B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：（株）サカタのタネ殿                    *
*    サブシステム　　：ＨＧ基幹システム苗業務連携              *
*    業務名　　　　　：                                        *
*    モジュール名　　：小売連携発注データ抽出（手書き）        *
*    作成日／更新日　：2011/10/31                              *
*    作成者／更新者　：飯田/NAV                                *
*    処理概要　　　　：                                        *
*      パラメタにて受取った抽出条件より、苗業務システムへ      *
*      送信する小売連携（手書）発注データを、                  *
*      売上伝票ファイルより抽出する。                          *
*      また、次処理への情報引き渡しのため、パラメタＢを設定する*
*      ※連携ナンバーは８桁までのカウントとする                *
*                                                              *
*　　更新日／更新者　　　：　2012/07/13                        *
*　　更新日／更新者　　　：　NAV INOUE                         *
*    修正概要　　　　　　：                                    *
*      売上伝票データの小売連携区分はノーチェックにする。      *
*      OUTPUTに項目追加（小売連携区分・商品名）。              *
*　　更新日／更新者　　　：　2012/08/30                        *
*　　更新日／更新者　　　：　NAV TAKAHAHSI                     *
*    修正概要　　　　　　：                                    *
*      売上伝票データの行番号＝８０は対象外に変更する。　      *
*　　更新日／更新者　　　：　2012/09/04 NAV TAKAHASHI          *
*    修正概要　　　　　　：                                    *
*      取引先マスタより、計上部門コードを受取り、計上部門コード*
*      を連携データにセットする。　　　　　　　　　　　　　　　*
*　　　計上部門→Ｆ０２、所属部門→Ｆ３２にセット　　　　　　　*
*　　更新日／更新者　　　：　2012/09/20,21                     *
*　　更新日／更新者　　　：　NAV TAKAHAHSI & INOUE             *
*    修正概要　　　　　　：                                    *
*      売上伝票データのＬＦを変更し速度ＵＰ。　　　　　　      *
*    作成日／更新日　：2013/01/25
*    作成者／更新者　：NAV TAKAHAHSI
*    修正概要　　管理番号を追加。
*　　　　　　　　データ存在チェック追加、パラメタ追加（出力）
*    作成日／更新日　：2013/02/07
*    作成者／更新者　：NAV TAKAHAHSI
*    修正概要　　ナフコの場合、転送する伝票番号の先頭３桁を
*　　　　　　　　店舗ＣＤに変換して送信する。
*    変更日／更新日　：2016/10/20                              *
*    変更内容　　　　：                                        *
*      小売連携商品のみを対象とする                            *
*      ２０１２／０７／１３対応分の戻し作業                    *
*    変更日／更新日　：2019/05/07                              *
*    変更内容　　　　：                                        *
*      取引先ＣＤ＝２２４３のとき、店舗ＣＤに７００を加算する  *
*    変更日／更新日　：2020/04/22                              *
*    変更内容　　　　：                                        *
*      直送分、セット組分の受注データ抽出（Ｄ３６５用）　　　　*
*    変更日／更新日　：2021/01/05                              *
*    変更内容　　　　：                                        *
*      返品データ対応　　　　　　　　　　　　　　　　　　　　　*
*    変更日／更新日　：2021/05/O6                              *
*    変更内容　　　　：                                        *
*      数量＝０対応（使用変更）　先行受注数／先行ＦＬＧセット　*
*    変更日／更新日　：2022/02/28 NAV TAKAHASHI                *
*    変更内容　　　　：実納品日を予備日付にセット              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0620B.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/10/31.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN      TO  SHTDENLF
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
         URI-F277 *> 売上データ作成
         URI-F274 *> オンライン
         URI-F01  *> 取引先コード
         URI-F08  *> 出荷場所
         URI-F02  *> 伝票番号
         URI-F04  *> 相殺区分
         URI-F051 *> 伝区コード
         URI-F07  *> 店舗コード
         URI-F112 *> 納品日
         URI-F03  *> 行番号
       FILE      STATUS    URI-ST.

*最新連携Ｎｏファイル
     SELECT  NARRNOF
       ASSIGN      TO  NARRNOL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         RNO-F01 *> ＫＥＹ
       FILE STATUS     RNO-ST.

*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01 *> 連携Ｎｏ
       FILE STATUS     KAN-ST.
*小売連携発注データ
     SELECT  TXXXXXXA
       ASSIGN    TO    TXXXXXXA
       ORGANIZATION    SEQUENTIAL
       FILE STATUS     TXX-ST.
*↓2012/09/04
*取引先マスタ
     SELECT  HTOKMS     ASSIGN    TO        TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    IS   TOK-ST.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    IS   MEI-ST.
*↑2012/09/04
*↓2020/04/22
*ＳＵＢ商品変換ＴＢＬ
     SELECT  SUBTBLF    ASSIGN    TO        SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01  TBL-F02
                        FILE      STATUS    IS   TBL-ST.
*↑2020/04/22
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   URI       AS   PREFIX.
****************************************************************
*    FILE = 最新連携Ｎｏファイル                               *
****************************************************************
 FD  NARRNOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARRNOF   OF   XFDLIB
                       JOINING   RNO       AS   PREFIX.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARKANF   OF   XFDLIB
                       JOINING   KAN       AS   PREFIX.
****************************************************************
*    FILE = 小売連携発注データ                                 *
****************************************************************
 FD  TXXXXXXA          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EXXXXXXA  OF   XFDLIB
                       JOINING   TXX       AS   PREFIX.
*↓2012/09/04
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS            LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
******************************************************************
*    商品名称マスタ（最新の連携区分を取得のため）
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*↑2012/09/04
*↓2020/04/22
******************************************************************
*    ＳＵＢ商品変換ＴＢＬ
******************************************************************
 FD  SUBTBLF            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   TBL       PREFIX.
*↑2020/04/22
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  URI-ST             PIC  X(02).
     03  RNO-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
     03  TXX-ST             PIC  X(02).
*↓2012/09/04
     03  TOK-ST             PIC  X(02).
     03  MEI-ST             PIC  X(02).
*↑2012/09/04
*↓2020/04/22
     03  TBL-ST        PIC  X(02).
*↑2020/04/22
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-SHTDENF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-NARKANF-INV     PIC  9(01)  VALUE  ZERO.
     03  FG-NARRNOF-INV     PIC  9(01)  VALUE  ZERO.
*↓2012/09/04
     03  HTOKMS-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG     PIC  X(03)     VALUE  SPACE.
*↑2012/09/04
*↓2020/04/22
     03  SUBTBLF-INV-FLG    PIC  X(03)  VALUE  SPACE.
*↑2020/04/22
*↓2020/04/22
 01  WK-JYUTYU-KBN           PIC  X(01)     VALUE  SPACE.
 01  WK-JYUTYU-CHK           PIC  X(01)     VALUE  SPACE.
*↑2020/04/22
*ワーク領域
 01  WRK-AREA.
     03  CT-IN              PIC  9(08).
     03  CT-OT              PIC  9(08).
     03  CT-OT2             PIC  9(08).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).

     03  IX                 PIC  9(04).
*
     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO REDEFINES WK-REN-NO-X
                            PIC  9(09).
*↓2012.4.18
     03  WK-REN-NO-R REDEFINES WK-REN-NO-X.
         05 WK-REN-NO-1     PIC  9(01).
         05 WK-REN-NO-8     PIC  9(08).
*↑2012.4.18
*
     03  WK-CVHZK-9         PIC  9(08).
     03  WK-CVHZK  REDEFINES  WK-CVHZK-9
                            PIC  X(08).
*
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH              PIC   9(02)  VALUE  ZERO.
     03  WK-MM              PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC   9(02)  VALUE  ZERO.

*↓2012.09.04
*部門コード退避
 01  WK-BUMON.
     03  WK-BUMON-1        PIC  9(04).
     03  WK-BUMON-2        PIC  9(08).
     03  WK-BUMON-3        PIC  9(04).
     03  WK-BUMON-4        PIC  X(04).
*↑2012.09.04
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

 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  RNO-ERR           PIC  N(20)  VALUE
         NC"最新連携Ｎｏファイルエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TXX-ERR           PIC  N(20)  VALUE
         NC"小売連携発注データエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタデータエラー".
*↓2020/04/22
     03  TBL-ERR           PIC  N(20)  VALUE
         NC"ＳＵＢ商品変換ＴＢＬエラー".
*↑2020/04/22
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
*2013/02/07 NAV ST
 01  WK-NAFUKO-DEN           PIC   9(09)  VALUE  ZERO.
*2013/02/07 NAV ED
*↓2019/05/07
 01  WK-URI-F07              PIC  9(05)     VALUE  ZERO.
*↑2019/05/07
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  PAR-IPADDR             PIC  X(15).
*  パラメータＡ
 01  PAA-TANCD              PIC  X(06).
 01  PAA-ONLTEG             PIC  X(01).
 01  PAA-BATNO              PIC  X(20).
 01  PAA-BASHO              PIC  X(02).
 01  PAA-NOHNBI             PIC  9(08).
 01  PAA-JNOHNBI            PIC  9(08).
 01  PAA-TORCD              PIC  9(08).
 01  PAA-DEN1-FROM          PIC  9(09).
 01  PAA-DEN1-TO            PIC  9(09).
 01  PAA-DEN2-FROM          PIC  9(09).
 01  PAA-DEN2-TO            PIC  9(09).
 01  PAA-DEN3-FROM          PIC  9(09).
 01  PAA-DEN3-TO            PIC  9(09).
 01  PAA-DEN4-FROM          PIC  9(09).
 01  PAA-DEN4-TO            PIC  9(09).
 01  PAA-DEN5-FROM          PIC  9(09).
 01  PAA-DEN5-TO            PIC  9(09).
 01  PAA-STS                PIC  X(01).
 01  PAA-RENKEINO           PIC  X(09).
*2013/01/25 NAV ST
 01  PAA-KANRINO            PIC  9(08).
*2013/01/25 NAV ED

* 出力パラメータ
*  パラメータＢ
 01  PAB-TANCD              PIC  X(06).
 01  PAB-ONLTEG             PIC  X(01).
 01  PAB-RENNO-FROM         PIC  X(09).
 01  PAB-RENNO-TO           PIC  X(09).
 01  PAB-IPADDR             PIC  X(15).
*#2020/04/22 NAV ST
 01  PARA-IN-JYUTYU-KBN     PIC   X(01).
*#2020/04/22 NAV ED

*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PAR-IPADDR
                                  PAA-TANCD
                                  PAA-ONLTEG
                                  PAA-BATNO
                                  PAA-BASHO
                                  PAA-NOHNBI
                                  PAA-JNOHNBI
                                  PAA-TORCD
                                  PAA-DEN1-FROM
                                  PAA-DEN1-TO
                                  PAA-DEN2-FROM
                                  PAA-DEN2-TO
                                  PAA-DEN3-FROM
                                  PAA-DEN3-TO
                                  PAA-DEN4-FROM
                                  PAA-DEN4-TO
                                  PAA-DEN5-FROM
                                  PAA-DEN5-TO
                                  PAA-STS
                                  PAA-RENKEINO
********************************2013/01/25 NAV ST
                                  PAA-KANRINO
********************************2013/01/25 NAV ED
                                  PAB-TANCD
                                  PAB-ONLTEG
                                  PAB-RENNO-FROM
                                  PAB-RENNO-TO
*#2020/04/22 NAV ST
****************************      PAB-IPADDR.
                                  PAB-IPADDR
                          PARA-IN-JYUTYU-KBN.
*#2020/04/22 NAV ED
**************************************************************
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
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
     USE         AFTER     EXCEPTION PROCEDURE TXXXXXXA.
     DISPLAY     TXX-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TXX-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↓2020/04/22
 TBL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SUBTBLF.
     DISPLAY     TBL-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TBL-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2012/09/04
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.

     DISPLAY   "**  START SNA0620B  **"  UPON CONS.

     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O   SHTDENF.
     OPEN  I-O   NARRNOF.
     OPEN  I-O   NARKANF.
     OPEN  OUTPUT TXXXXXXA.
*↓2012/09/04
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HMEIMS.
*↑2012/09/04
*↓2020/04/23
     MOVE     PARA-IN-JYUTYU-KBN    TO    WK-JYUTYU-KBN.
*↑2030/04/23
*#2020/04/22 NAV ST
     MOVE     PARA-IN-JYUTYU-KBN       TO   WK-JYUTYU-KBN.
     MOVE     PARA-IN-JYUTYU-KBN       TO   WK-JYUTYU-CHK.
     DISPLAY NC"＃＃抽出オーダー区分" " = " WK-JYUTYU-CHK
             UPON CONS.
*****EVALUATE WK-JYUTYU-KBN
*        WHEN "1"  DISPLAY NC"＃先行直送分＃" UPON  CONS
*                  MOVE "2"            TO   WK-JYUTYU-CHK
*        WHEN "2"  DISPLAY NC"＃セット組分＃" UPON  CONS
*                  MOVE "3"            TO   WK-JYUTYU-CHK
*        WHEN OTHER DISPLAY NC"＃対象無し＃" UPON  CONS
*****END-EVALUATE.
*#2020/04/22 NAV ED
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.

     INITIALIZE  URI-REC.
     MOVE  LOW-VALUE        TO  FG-SHTDENF-END.

*↓2012.09.04 部門ワーク初期化／取引先マスタ読込（部門取得）
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
     INITIALIZE                   WK-BUMON.
*取引先マスタ読込（部門コード取得）
     MOVE     PAA-TORCD          TO   TOK-F01.
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

     PERFORM  RD-SHTDENF-SEC.  *> 売上伝票の初期読込み
     IF  FG-SHTDENF-END = "END"
         MOVE  "END"        TO  END-FLG
         GO TO  INIT-EXIT
     END-IF.

     PERFORM  RENNO-GET-SEC. *> 連携Ｎｏを採番

 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*    連携Ｎｏ採番
****************************************************************
 RENNO-GET-SEC              SECTION.
     MOVE  "RENNO-GET-SEC"       TO  S-NAME.
*
     MOVE  1                TO  RNO-F01.

     READ  NARRNOF
       INVALID
         MOVE  1            TO  FG-NARRNOF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARRNOF-INV
     END-READ.

     IF  FG-NARRNOF-INV = 1
         INITIALIZE  RNO-REC
         MOVE  1            TO  RNO-F01
         MOVE  ZERO         TO  RNO-F02
     END-IF.

     IF RNO-F02 IS NUMERIC
        MOVE  RNO-F02       TO  WK-REN-NO-X
     ELSE
        MOVE  ZERO          TO  WK-REN-NO
     END-IF.

*↓2012.4.18
*****COMPUTE  WK-REN-NO = WK-REN-NO + 1.
*****IF WK-REN-NO = ZERO
     COMPUTE  WK-REN-NO-8 = WK-REN-NO-8 + 1.
     IF WK-REN-NO-8 = ZERO
        MOVE  1             TO  WK-REN-NO
     END-IF.
*
     MOVE  WK-REN-NO-X      TO  RNO-F02.
*
     MOVE  SYS-DATE         TO  RNO-F98.
     MOVE  WK-TIME(1:6)     TO  RNO-F99

     IF  FG-NARRNOF-INV = ZERO
         REWRITE  RNO-REC
     ELSE
         WRITE  RNO-REC
     END-IF.

* 採番した連携Ｎｏをコンソールに表示。
     DISPLAY  MSG-WAKU  UPON CONS.
     DISPLAY  MSG-01    UPON CONS.
     MOVE  WK-REN-NO-X      TO  MSG-02-RENNO.
     DISPLAY  MSG-02    UPON CONS.
     DISPLAY  MSG-WAKU  UPON CONS.

 RENNO-GET-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込み　                                  *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
     MOVE  "RD-SHTDENF-SEC"      TO  S-NAME.

     IF  FG-SHTDENF-END = LOW-VALUE
         MOVE   ZERO         TO  URI-F277
         MOVE   ZERO         TO  URI-F274
         MOVE   PAA-TORCD    TO  URI-F01
         MOVE   PAA-BASHO    TO  URI-F08
         START  SHTDENF  KEY >=  URI-F277 *> 売上データ作成
                                 URI-F274 *> オンライン
                                 URI-F01  *> 取引先コード
                                 URI-F08  *> 出荷場所
                                 URI-F02  *> 伝票番号
                                 URI-F04  *> 相殺区分
                                 URI-F051 *> 伝区コード
                                 URI-F07  *> 店舗コード
                                 URI-F112 *> 納品日
                                 URI-F03  *> 行番号
           INVALID KEY
              MOVE  "END"   TO  FG-SHTDENF-END
              GO TO  RD-SHTDENF-EXIT
         END-START

         MOVE  SPACE        TO  FG-SHTDENF-END

     END-IF.

     READ  SHTDENF
       AT  END
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-READ.

* 売上データ作成
     IF  URI-F277 = ZERO *>未計上データ
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
* オンライン
     IF  URI-F274 = ZERO *>手書き伝票
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
* URI-F01 : 取引先コード
     IF  URI-F01 = PAA-TORCD
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
* URI-F48 : 出荷場所
     IF  URI-F08 = PAA-BASHO
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
* URI-F04 : 相殺区分
* URI-F051: 伝区コード
     IF  URI-F04 = ZERO  AND  URI-F051 = 41
         CONTINUE
     ELSE
         GO TO  RD-SHTDENF-SEC
     END-IF.
*
* URI-F03 : 行番号
     IF      URI-F03 < 80
         CONTINUE
     ELSE
         GO TO  RD-SHTDENF-SEC
     END-IF.
*
* URI-F15 : 出荷数量
     IF      URI-F15 > ZERO
         CONTINUE
     ELSE
         GO TO  RD-SHTDENF-SEC
     END-IF.
*
* 取引先、出荷場所
*##  IF      URI-F01 = PAA-TORCD
*********2012/07/26↓判定を振分倉庫→出荷場所へ変更する
*********          ↓手書の場合は、出荷場所で判定する。
*##      AND URI-F08 = PAA-BASHO
*T       AND URI-F48 = PAA-BASHO
*********2012/07/26↑判定を振分倉庫→出荷場所へ変更する
*********2012/08/30↓行番号＝８０は対象外とする。
*##      AND URI-F03 < 80
*********2012/08/30↑行番号＝８０は対象外とする。
*#       CONTINUE
*#   ELSE
*#       GO TO  RD-SHTDENF-SEC
*#   END-IF.
*
* 伝票番号（範囲）
     EVALUATE  TRUE
       WHEN      URI-F02 >= PAA-DEN1-FROM
             AND URI-F02 <= PAA-DEN1-TO
         CONTINUE
       WHEN      URI-F02 >= PAA-DEN2-FROM
             AND URI-F02 <= PAA-DEN2-TO
         CONTINUE
       WHEN      URI-F02 >= PAA-DEN3-FROM
             AND URI-F02 <= PAA-DEN3-TO
         CONTINUE
       WHEN      URI-F02 >= PAA-DEN4-FROM
             AND URI-F02 <= PAA-DEN4-TO
         CONTINUE
       WHEN      URI-F02 >= PAA-DEN5-FROM
             AND URI-F02 <= PAA-DEN5-TO
         CONTINUE
       WHEN  OTHER
         GO TO  RD-SHTDENF-SEC
     END-EVALUATE.
*
*↓2012/07/13 小売連携区分は無視する
*  ↓2016/10/20 ST 小売連携区分＝１の明細のみ連携に変更
     IF  URI-F32 = WK-JYUTYU-CHK *> 小売連携区分あり
     AND URI-F33 = SPACE       *> 小売連携_未採番
         CONTINUE
**** IF  URI-F33 = SPACE       *> 小売連携_未採番
****     CONTINUE
*  ↑2016/10/20 ED 小売連携区分＝１の明細のみ連携に変更
*↑2012/07/13
     ELSE
         GO TO  RD-SHTDENF-SEC
     END-IF.
*2013/01/25 NAV ST
*#2020/03/08 NAV ST ナフコ関係は停止する
*****IF  URI-F01 = 137607
*        IF  PAA-KANRINO = URI-F413
*            CONTINUE
*        ELSE
*            GO TO  RD-SHTDENF-SEC
*        END-IF
*****END-IF.
*#2020/03/08 NAV ED ナフコ関係は停止する
*2013/01/25 NAV ED
*
**     DISPLAY "SHTDENLC UD="  URI-F277
**                       ",ONTE="  URI-F274
**                       ",HBAS="  URI-F09
**                       ",TOR="  URI-F01
**                       ",DEN="  URI-F02
**                       ",SOS="  URI-F04
**                       ",DNK="  URI-F051
**                       ",TEN="  URI-F07
**                       ",NOH="  URI-F112
**                       ",GYO="  URI-F03
**                       ",BASHO=" URI-F08
**                       "*"
**       UPON CONS.

     ADD 1   TO  CT-IN.

 RD-SHTDENF-EXIT.
     EXIT.

****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
     PERFORM  UNTIL FG-SHTDENF-END = "END"
       PERFORM  TXXXXXXA-EDWT-SEC
       PERFORM  UP-SHTDENF-SEC
       PERFORM  RD-SHTDENF-SEC
     END-PERFORM.
*
     PERFORM  NARKANF-EDWT-SEC.
*
     MOVE  "END"            TO  END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*  編集／出力処理                                              *
****************************************************************
 TXXXXXXA-EDWT-SEC               SECTION.
     MOVE  "TXXXXXXA-EDWT-SEC"       TO  S-NAME.
*
     INITIALIZE  TXX-REC.
     MOVE  WK-REN-NO-X      TO  TXX-F01. *> 連携NO
*↓2012/09/04
*****MOVE  PAA-TANCD (1:4)  TO  TXX-F02. *> 部門コード
     MOVE  WK-BUMON-4       TO  TXX-F02. *> 部門コード
*↑2012/09/04
     MOVE  PAA-TANCD (5:2)  TO  TXX-F03. *> 担当者コード
*****MOVE  PAA-TORCD        TO  TXX-F04. *> 得意先コード
*****MOVE  URI-F24          TO  TXX-F05. *> 自社得意先コード
     MOVE  URI-F24          TO  TXX-F04. *> 得意先コード
     MOVE  PAA-TORCD        TO  TXX-F05. *> 自社得意先コード
     MOVE  ZERO             TO  TXX-F06. *> バッチNO
     MOVE  URI-F111         TO  WK-CVHZK-9
     MOVE  WK-CVHZK         TO  TXX-F07. *> 注文日
     MOVE  PAA-NOHNBI       TO  TXX-F08. *> 納品日
     MOVE  PAA-JNOHNBI      TO  TXX-F09. *> 実納品日
     MOVE  "0"              TO  TXX-F10(1:1). *> 店舗コード
     MOVE  URI-F07          TO  TXX-F10(2:5). *> 店舗コード
*↓2019/05/07 NAV ST ジョイフル本田対応
     IF       URI-F01  =  2243
     AND      URI-F07  <  7000
              COMPUTE  WK-URI-F07  =  URI-F07  +  7000
              MOVE     "0"                 TO   TXX-F10(1:1)
              MOVE     WK-URI-F07          TO   TXX-F10(2:5)
     END-IF.
*↑2019/05/07 NAV ED ジョイフル本田対応
     MOVE  URI-F051         TO  TXX-F11. *> 伝票区分
     MOVE  URI-F02          TO  TXX-F12. *> 自社伝票番号
     MOVE  URI-F23          TO  TXX-F13. *> 相手伝票番号
*2013/02/07 NAV ST  ナフコの場合、特別対応
*#2020/03/08 NAV ST ナフコ関係は停止する
*****IF    URI-F01  =  137607
*          COMPUTE  WK-NAFUKO-DEN  =  URI-F07  *  1000000
*          COMPUTE  WK-NAFUKO-DEN  =  WK-NAFUKO-DEN + URI-F02
*          MOVE     WK-NAFUKO-DEN  TO TXX-F12  TXX-F13
*    END-IF.
*#2020/03/08 NAV ED ナフコ関係は停止する
*2013/02/07 NAV ED  ナフコの場合、特別対応
     MOVE  PAR-IPADDR       TO  TXX-F14. *> 送信元端末ＩＰ
     MOVE  PAA-ONLTEG       TO  TXX-F15. *> 入力区分
     MOVE  URI-F42          TO  TXX-F16. *> ルート
     MOVE  URI-F03          TO  TXX-F17. *> 伝票行番号
*****2012/07/26↓振分倉庫から出荷場所へ変更する。
***I MOVE  PAA-BASHO        TO  TXX-F18. *> 出荷場所コード
***T MOVE  URI-F48          TO  TXX-F18. *> 出荷場所コード
     MOVE  URI-F08          TO  TXX-F18. *> 出荷場所コード
*****2012/07/26↑振分倉庫から出荷場所へ変更する。
     MOVE  URI-F1411        TO  TXX-F19. *> 自社商品コード
     MOVE  URI-F1412        TO  TXX-F20. *> 品単コード
*# 2020/05/01 NAV ST Ｄ３６５連携対応
*  商品名称マスタからの再取得を停止する。
*↓2012/09/04 NAV ST
*****商品名称マスタを読込、連携区分を再取得する。
**** PERFORM HMEIMS-READ-SEC.
*    IF    HMEIMS-INV-FLG = "INV"
*          CONTINUE
*    ELSE
*          MOVE  MEI-F10    TO  URI-F32
*    END-IF.
*↑2012/09/04 NAV ED
*# 2020/05/01 NAV ED Ｄ３６５連携対応
     MOVE  URI-F25          TO  TXX-F21. *> 相手商品コード
     MOVE  URI-F15          TO  TXX-F22. *> 数量
     MOVE  URI-F172         TO  TXX-F23. *> 原価単価
     MOVE  URI-F173         TO  TXX-F24. *> 売価単価
     MOVE  URI-F181         TO  TXX-F25. *> 原価金額
     MOVE  URI-F182         TO  TXX-F26. *> 売価金額
     MOVE  URI-F22          TO  TXX-F27. *> 明細備考
     IF  URI-F03 = 80 *> 行番号
         MOVE  URI-F1421    TO  TXX-F28  *> 伝票備考
     ELSE
         MOVE  SPACE        TO  TXX-F28  *> 伝票備考
     END-IF.
*↓2012/07/13
     MOVE  URI-F32          TO  TXX-F29. *> 小売連携区分
     MOVE  URI-F1421        TO  TXX-F30. *> 商品名（カナ）１
     MOVE  URI-F1422        TO  TXX-F31. *> 商品名（カナ）２
*↑2012/07/13
*↓2012/09/04
     MOVE  PAA-TANCD (1:4)  TO  TXX-F32. *> 小売連携区分
*↑2012/09/04
     MOVE  "1"              TO  TXX-F97. *> 処理区分
                                         *> (取込み依頼)
     MOVE  SYS-DATE         TO  TXX-F98. *> 依頼日付
     MOVE  WK-TIME          TO  TXX-F99. *> 依頼時刻
*
     WRITE  TXX-REC.
*
     ADD 1   TO  CT-OT2.
*
 TXXXXXXA-EDWT-EXIT.
     EXIT.
****************************************************************
*  売上伝票ファイル　連携_更新                                *
****************************************************************
 UP-SHTDENF-SEC                  SECTION.
     MOVE  "UP-SHTDENF-SEC"       TO  S-NAME.
*
     MOVE  WK-REN-NO-X      TO  URI-F33. *> 連携NO
*# 2020/05/01 NAV ST Ｄ３６５連携対応
*****MOVE  "1"              TO  URI-D95. *> 先行受注区分
*# 2020/05/01 NAV ED Ｄ３６５連携対応
*# 2021/05/06 NAV ST Ｄ３６５連携対応　数量＝０対応
     MOVE  URI-F15          TO  URI-D86.
     MOVE  "1"              TO  URI-D88.
*# 2021/05/06 NAV ED Ｄ３６５連携対応　数量＝０対応
*# 2022/02/28 NAV ST 実納品日を予備日付にセット　　　
     MOVE  PAA-JNOHNBI      TO  URI-D87.
*# 2022/02/28 NAV ED
*
     REWRITE  URI-REC.
*
 UP-SHTDENF-EXIT.
     EXIT.
****************************************************************
*  連携Ｎｏ管理テーブル編集／出力処理                          *
****************************************************************
 NARKANF-EDWT-SEC              SECTION.
     MOVE  "NARKANF-EDWT-SEC"       TO  S-NAME.
*
     MOVE  WK-REN-NO-X      TO  KAN-F01.

     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.

     INITIALIZE  KAN-REC.
     MOVE  WK-REN-NO-X      TO  KAN-F01.  *> 連携Ｎｏ
     MOVE  PAA-TANCD (1:4)  TO  KAN-F02.  *> 部門コード
     MOVE  PAA-TANCD (5:2)  TO  KAN-F03.  *> 担当者コード
     MOVE  "1"              TO  KAN-F04.  *> 状態FLG(抽出済)
     MOVE  SPACE            TO  KAN-F05.  *> 前回状態FLG
     MOVE  CT-IN            TO  KAN-F06.  *> データ件数
     MOVE  PAA-ONLTEG       TO  KAN-F071. *> 入力区分
     MOVE  PAA-BATNO        TO  KAN-F072. *> バッチＮｏ
     MOVE  PAA-BASHO        TO  KAN-F073. *> 場所コード
     MOVE  PAA-NOHNBI       TO  KAN-F074. *> 納品日
     MOVE  PAA-JNOHNBI      TO  KAN-F075. *> 実納品日
     MOVE  PAA-TORCD        TO  KAN-F076. *> 取引先コード
     MOVE  PAA-DEN1-FROM    TO  KAN-F077. *> 伝票NO1(FROM)
     MOVE  PAA-DEN1-TO      TO  KAN-F078. *> 伝票NO1(TO)
     MOVE  PAA-DEN2-FROM    TO  KAN-F079. *> 伝票NO2(FROM)
     MOVE  PAA-DEN2-TO      TO  KAN-F07A. *> 伝票NO2(TO)
     MOVE  PAA-DEN3-FROM    TO  KAN-F07B. *> 伝票NO3(FROM)
     MOVE  PAA-DEN3-TO      TO  KAN-F07C. *> 伝票NO3(TO)
     MOVE  PAA-DEN4-FROM    TO  KAN-F07D. *> 伝票NO4(FROM)
     MOVE  PAA-DEN4-TO      TO  KAN-F07E. *> 伝票NO4(TO)
     MOVE  PAA-DEN5-FROM    TO  KAN-F07F. *> 伝票NO5(FROM)
     MOVE  PAA-DEN5-TO      TO  KAN-F07G. *> 伝票NO5(TO)
     MOVE  SYS-DATE         TO  KAN-F96.  *> 依頼日付
     MOVE  WK-TIME(1:6)     TO  KAN-F97.  *> 依頼時刻
     MOVE  ZERO             TO  KAN-F98.  *> 返答日付
     MOVE  ZERO             TO  KAN-F99.  *> 返答時刻

     IF  FG-NARKANF-INV = 1
         WRITE  KAN-REC
     ELSE
         REWRITE  KAN-REC
     END-IF.

     ADD 1   TO  CT-OT.

 NARKANF-EDWT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
     PERFORM  PARA-OT-SEC.
*ファイル ＣＬＯＳＥ
     CLOSE  SHTDENF.
     CLOSE  NARRNOF.
     CLOSE  NARKANF.
     CLOSE  TXXXXXXA.
*↓2012/09/04
     CLOSE  HTOKMS.
     CLOSE  HMEIMS.
*↑2012/09/04
*↓2020/04/22
     CLOSE     HTOKMS.
*↑2020/04/22
     DISPLAY   "**  SHTDENF  IN = " CT-IN   UPON CONS.
     DISPLAY   "**  NARKANF  OT = " CT-OT   UPON CONS.
     DISPLAY   "**  TXXXXXXA OT = " CT-OT2  UPON CONS.
     DISPLAY   "**  END   SNA0620B  **"  UPON CONS.

 END-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
     MOVE  "PARA-OT-SEC"       TO  S-NAME.

* 部門・担当者ＣＤ
     MOVE  PAA-TANCD        TO  PAB-TANCD.
* オンライン/手書種別
     MOVE  PAA-ONLTEG       TO  PAB-ONLTEG.
     IF  CT-IN NOT = ZERO
         *> 連携NO(FROM)
         MOVE  WK-REN-NO-X  TO  PAB-RENNO-FROM
         *> 連携NO(TO)
         MOVE  WK-REN-NO-X  TO  PAB-RENNO-TO
     ELSE
         MOVE  SPACE        TO  PAB-RENNO-FROM
         MOVE  SPACE        TO  PAB-RENNO-TO
     END-IF.
* 起動元端末IP
     MOVE  PAR-IPADDR       TO  PAB-IPADDR.

 PARA-OT-EXIT.
     EXIT.
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
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     MOVE     URI-F1411     TO         MEI-F011.
     MOVE     URI-F1412     TO         MEI-F012.
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*↑2012/09/04
*****************<<  SNA0620B   END PROGRAM  >>******************

```
