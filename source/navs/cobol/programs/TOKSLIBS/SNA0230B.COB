***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：連携Ｎｏ取込み結果更新
*    作成日／更新日　：2011/11/01
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへ取込依頼を行った結果を受け、
*      売上伝票ファイル・連携Ｎｏ管理テーブル・小売連携累積
*      ファイルに対し、その結果更新処理を行う。
*    作成日／更新日　：2012/07/17
*    作成者／更新者　：井上/NAV
*    処理概要　　　　：
*      売上伝票ファイルのキー変更（小売連携区分をカット）
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0230B.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS
     YA         IS  PIT-2
     YA-21      IS  PIT-2-2W
     YA-22      IS  PIT-2-2W2H
     YB         IS  PIT-1V5
     YB-21      IS  PIT-1V5-2W
     YB-22      IS  PIT-1V5-2W2H.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*結果通知データ（取込）SF
     SELECT  NARKTRF
       ASSIGN      TO  NARKEKF
       ORGANIZATION    SEQUENTIAL
       FILE      STATUS    KTR-ST.
*
*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN      TO  SHTDENLE
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
*↓2012/07/17
****     URI-F32  *> 小売連携区分
*↑2012/07/17
         URI-F33  *> 連携Ｎｏ
         URI-F112 *> 納品日
       FILE      STATUS    URI-ST.
*
*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01 *> 連携Ｎｏ
       FILE STATUS     KAN-ST.
*
*小売連携累積ファイル
     SELECT  NARRUIF
       ASSIGN      TO  NARRUIL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
         RUI-F01 *> ＫＥＹ
       FILE STATUS     RUI-ST.
*
*担当者マスタ
     SELECT  HTANMS
       ASSIGN    TO    TANMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TAN-F01 *> 部門ＣＤ
         TAN-F02 *> 担当者ＣＤ
       FILE STATUS     TAN-ST.
*
*小売連携結果リスト
     SELECT  PRTFILE
       ASSIGN    TO  LP-04
       FILE  STATUS  PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 結果通知データ（取込）SF                           *
****************************************************************
 FD  NARKTRF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKTRF   OF   XFDLIB
     JOINING   KTR       AS   PREFIX.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
     LABEL     RECORD    IS   STANDARD.
     COPY      SHTDENF   OF   XFDLIB
     JOINING   URI       AS   PREFIX.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
****************************************************************
*    FILE = 小売連携累積ファイル                               *
****************************************************************
 FD  NARRUIF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARRUIF   OF   XFDLIB
     JOINING   RUI       AS   PREFIX.
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD  HTANMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTANMS    OF   XFDLIB
     JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = プリントファイル                                   *
****************************************************************
 FD  PRTFILE
     LABEL RECORD OMITTED.
*
 01  PRT-REC.
     03  FILLER             PIC  X(300).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KTR-ST             PIC  X(02).
     03  URI-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
     03  RUI-ST             PIC  X(02).
     03  TAN-ST             PIC  X(02).
     03  PRT-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-SHTDENF-END     PIC  X(03).
     03  FG-NARKTRF-END     PIC  X(03).
     03  FG-NARRUIF-END     PIC  X(03).
     03  FG-NARKANF-INV     PIC  9(01).
     03  FG-HTANMS-INV      PIC  9(01).
     03  NASI-FLG           PIC  X(03)  VALUE  SPACE.
*↓
     03  ERR-FLG            PIC  X(01)  VALUE  SPACE.
     03  JOUTAI-FLG         PIC  X(03)  VALUE  SPACE.
*↑
*ワーク領域
 01  WRK-AREA.
     03  CT-IN0             PIC  9(08).
     03  CT-IN              PIC  9(08).
     03  CT-IN2             PIC  9(08).
     03  CT-UP              PIC  9(08).
     03  CT-DL              PIC  9(08).
     03  CT-PG              PIC  9(08).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).
*
     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).
*
     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).
*
     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).
*
     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.
*
 01  WK-RENNO-G.
     03  WK-RENNO           PIC  X(09).
     03  WK-RENNOR-9  REDEFINES  WK-RENNO
                            PIC  9(09).
     03  WK-RENNOR    REDEFINES  WK-RENNO
                            PIC  X(01)  OCCURS 9.
*
     03  WK-RENNO-N         PIC  N(09).
     03  WK-RENNO-NR  REDEFINES  WK-RENNO-N
                            PIC  N(01)  OCCURS 9.
*
 01  WK-DTKENSU-G.
     03  WK-DTKENSU         PIC  X(06).
     03  WK-DTKENSUR-9  REDEFINES  WK-DTKENSU
                            PIC  9(06).
     03  WK-DTKENSUR-Z  REDEFINES  WK-DTKENSU
                            PIC  ZZZZZ9.
     03  WK-DTKENSUR    REDEFINES  WK-DTKENSU
                            PIC  X(01)  OCCURS 6.
*
     03  WK-DTKENSU-N       PIC  N(06).
     03  WK-DTKENSU-NR  REDEFINES  WK-DTKENSU-N
                            PIC  N(01)  OCCURS 6.
*
*  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*　システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*　日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC  9(02)  VALUE  ZERO.
*
 01  MSG-WAKU               PIC  N(25)  VALUE
     NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  MSG01.
     03  MSG011             PIC  N(25)  VALUE
     NC"＊　以下の連携Ｎｏは管理テーブルに存在しません　＊".
     03  MSG012.
       05  FILLER           PIC  X(22)  VALUE
       "＊　　　　　　　　　".
       05  MSG012-RENNO     PIC  X(09) VALUE SPACE.
       05  FILLER           PIC  X(01) VALUE SPACE.
       05  FILLER           PIC  X(22)  VALUE
       "　　　　　　　　　＊".
 01  MSG-WAKU2              PIC  N(26)  VALUE
     NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  MSG02.
     03  MSG021.
       05  FILLER           PIC  X(40)  VALUE
       "＊　売上伝票ファイル　クリア件数　＝　".
       05  MSG021-KENSU     PIC  9(06).
       05  FILLER           PIC  X(10)  VALUE
       "　件　＊".
     03  MSG022.
       05  FILLER           PIC  X(40)  VALUE
       "＊　連携累積ファイル　　削除件数　＝　".
       05  MSG022-KENSU     PIC  9(06).
       05  FILLER           PIC  X(10)  VALUE
       "　件　＊".
*↓
 01  MSG-WAKU3              PIC  N(25)  VALUE
     NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  MSG03.
     03  MSG0311            PIC  N(25)  VALUE
     NC"＊　取込依頼していない連携_の取込結果を、　　　＊".
     03  MSG0312            PIC  N(25)  VALUE
     NC"＊　苗業務システムから受け取りました！　　　　　＊".
     03  MSG032.
       05  FILLER           PIC  X(22)  VALUE
       "＊　　　　　　　　　".
       05  MSG032-RENNO     PIC  X(09) VALUE SPACE.
       05  FILLER           PIC  X(01) VALUE SPACE.
       05  FILLER           PIC  X(22)  VALUE
       "　　　　　　　　　＊".
*↑
 01  HEAD01.
     03  FILLER  PIC  X(08)  VALUE "SNA0230B".
     03  FILLER  PIC  X(27)  VALUE SPACE.
     03  FILLER  PIC  N(11)  VALUE
     NC"＜小売連携結果リスト＞"
                      CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(34)  VALUE SPACE.
     03  HD01-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  HD01-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  HD01-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE NC"頁"
                      CHARACTER TYPE IS PIT-2.
*
 01  HEAD02.
     03  FILLER  PIC  X(115)  VALUE SPACE.
     03  HD02-HH PIC  9(02).
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  HD02-MM PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  HD02-DD PIC  Z9.
*
 01  YKSEN.
     03  FILLER  PIC  X(135)  VALUE ALL "-".
*
 01  MEISAI01  CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(04)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"連携_　".
     03  MS01-RENNO   PIC  N(09).
     03  FILLER  PIC  N(03)  VALUE NC"　の（".
     03  FILLER  PIC  N(06)  VALUE NC"　取込結果　".
     03  FILLER  PIC  N(09)  VALUE NC"）を　受信しました".
*
 01  MEISAI01-2.
     03  FILLER  PIC  X(40)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"結　果"
                      CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS01-2-HC-KEKKA  PIC  N(03)
                      CHARACTER TYPE IS PIT-2-2W2H.
*
 01  MEISAI02.
     03  FILLER  PIC  X(40)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"発注種別　："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS02-HC-SYUBT  PIC  N(05)
                      CHARACTER TYPE IS PIT-1V5-2W.
*
 01  MEISAI03.
     03  FILLER  PIC  X(40)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"送信件数　："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS03-SND-KENSU PIC  N(06)
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  N(01)  VALUE NC"件"
                      CHARACTER TYPE IS PIT-1V5-2W.
*
 01  MEISAI04.
     03  FILLER          PIC  X(40)  VALUE SPACE.
     03  FILLER          PIC  N(06)  VALUE NC"送信担当者："
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  X(01)  VALUE SPACE.
     03  MS04-SND-BUMN   PIC  X(04).
     03  FILLER          PIC  X(01)  VALUE "-".
     03  MS04-SND-TANT   PIC  X(02).
     03  FILLER          PIC  X(01)  VALUE SPACE.
     03  MS04-SND-TANTNM PIC  N(10)
                              CHARACTER TYPE IS PIT-1V5-2W.
*
 01  MEISAI05.
     03  FILLER          PIC  X(19)  VALUE SPACE.
     03  MS05-MSG        PIC  N(32)  VALUE SPACE
                              CHARACTER TYPE IS PIT-1V5-2W.
 01  MEISAI05-MSG.
     03  FILLER          PIC  N(16)  VALUE
     NC"この連携_は、苗業務システムで"
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  N(16)  VALUE
     NC"取込できないため取り消されました"
                              CHARACTER TYPE IS PIT-1V5-2W.
**** 03  FILLER          PIC  N(32)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
**** NC"この連携_は、苗業務システムで取込できないため取り消され
*-      "ました。".
*
 01  MEISAI06.
     03  FILLER          PIC  X(19)  VALUE SPACE.
     03  MS06-MSG        PIC  N(32)  VALUE SPACE
                              CHARACTER TYPE IS PIT-1V5-2W.
*
 01  MEISAI06-MSG.
     03  FILLER          PIC  N(32)  VALUE
     NC"確認の上、再連携が必要です。".
*
 01  MEISAI07-MSG.
     03  FILLER          PIC  N(32)  VALUE
     NC"この連携_は、連携_管理テーブルに存在しません！　　　".
*      "　　　　".
*
 01  MEISAI08-MSG.
     03  FILLER          PIC  N(32)  VALUE
     NC"確認してください。　　　　　".
*↓
 01  MEISAI09-MSG.
     03  FILLER          PIC  N(16)  VALUE
     NC"取込依頼していない連携_の取込結"
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  N(16)  VALUE
     NC"果を苗業務システムから受取ました"
                              CHARACTER TYPE IS PIT-1V5-2W.
*↑
 01  FILE-ERR.
     03  KTR-ERR           PIC  N(20)  VALUE
         NC"結果通知データ（取込）エラー".
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  RUI-ERR           PIC  N(20)  VALUE
         NC"小売連携累積ファイルエラー".
     03  TAN-ERR           PIC  N(20)  VALUE
         NC"担当者マスタエラー".
     03  PRT-ERR           PIC  N(20)  VALUE
         NC"プリントファイルエラー".
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
*
* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KTR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKTRF.
     DISPLAY     KTR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KTR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RUI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRUIF.
     DISPLAY     RUI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RUI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRJ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNA0230B  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  INPUT  NARKTRF.
     OPEN  I-O    SHTDENF.
     OPEN  I-O    NARKANF.
     OPEN  I-O    NARRUIF.
     OPEN  INPUT  HTANMS.
     OPEN  OUTPUT PRTFILE.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.
*
     DISPLAY
        NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
                                                     UPON CONS.
*
     MOVE  SPACE            TO  FG-NARKTRF-END.
     PERFORM  RD-NARKTRF-SEC.
     IF  FG-NARKTRF-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
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
*    結果通知データ（取込）読込み　                            *
****************************************************************
 RD-NARKTRF-SEC        SECTION.
     MOVE  "RD-NARKTRF-SEC" TO  S-NAME.
*
     READ  NARKTRF
       AT  END
         MOVE  "END"        TO  FG-NARKTRF-END
         GO TO  RD-NARKTRF-EXIT
     END-READ.
*連携_＝空白は読み飛ばし
     IF  KTR-F01 = SPACE
         GO TO  RD-NARKTRF-SEC
     END-IF.
*
     ADD 1   TO  CT-IN0.
*
     DISPLAY NC"連携_" "=" KTR-F01 " " NC"結果" "=" KTR-F05
                                                     UPON CONS.
*
 RD-NARKTRF-EXIT.
     EXIT.
*
***************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     PERFORM  UPD-SEC.

     PERFORM  RD-NARKTRF-SEC.
     IF  FG-NARKTRF-END = "END"
         MOVE  CT-UP        TO  MSG021-KENSU *> クリア件数
         MOVE  CT-DL        TO  MSG022-KENSU *> 削除件数
         *> 処理件数表示
         DISPLAY  MSG-WAKU2  UPON CONS
         DISPLAY  MSG021     UPON CONS
         DISPLAY  MSG022     UPON CONS
         DISPLAY  MSG-WAKU2  UPON CONS
         MOVE  "END"        TO  END-FLG
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  更新処理                                                    *
****************************************************************
 UPD-SEC               SECTION.
     MOVE  "UPD-SEC"        TO  S-NAME.

     PERFORM  UPDB-SEC.

     PERFORM  PRT-SEC.

 UPD-EXIT.
     EXIT.
****************************************************************
*  更新Ｂ処理                                                  *
****************************************************************
 UPDB-SEC            SECTION.
     MOVE  "UPDB-SEC"       TO  S-NAME.

     IF  KTR-F05 = "OK"  *> 結果ＣＤ
         PERFORM  UPDC-SEC  *> ＯＫ更新
     ELSE
         PERFORM  UPDC2-SEC *> ＮＧ更新
     END-IF.

 UPDB-EXIT.
     EXIT.
****************************************************************
*  更新Ｃ処理 （ＯＫ更新）
****************************************************************
 UPDC-SEC            SECTION.
     MOVE  "UPDC-SEC"       TO  S-NAME.
     MOVE   KTR-F01         TO  KAN-F01.
*↓
     MOVE   SPACE           TO  ERR-FLG.
*↑

     PERFORM  NARKANF-RD-SEC.
     IF  FG-NARKANF-INV = 1
         PERFORM  NARKANF-NASI-SEC
         GO   TO  UPDC-EXIT
     END-IF.
*
*↓
     IF  KAN-F04 NOT = 2
         PERFORM JOUTAI-ERR-SEC
         MOVE    1   TO  ERR-FLG
         GO   TO  UPDC-EXIT
     END-IF.
*↑
     MOVE  KAN-F04          TO  KAN-F05. *> 前回状態FLG
     MOVE  "3"              TO  KAN-F04. *> 状態FLG(取込済)
     MOVE  KTR-F98          TO  KAN-F98. *> 返答日付
     MOVE  KTR-F99          TO  KAN-F99. *> 返答時刻
     REWRITE  KAN-REC.

 UPDC-EXIT.
     EXIT.
****************************************************************
*  小売連携管理テーブル検索                                    *
****************************************************************
 NARKANF-RD-SEC        SECTION.
     MOVE  "NARKANF-RD-SEC" TO  S-NAME.

     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.

 NARKANF-RD-EXIT.
     EXIT.
****************************************************************
*  小売連携管理テーブルレコード無し処理                        *
****************************************************************
 NARKANF-NASI-SEC       SECTION.
     MOVE  "NARKANF-NASI-SEC"  TO  S-NAME.
* レコード無し表示
     DISPLAY  MSG-WAKU  UPON CONS.
     DISPLAY  MSG011    UPON CONS.
     MOVE  KTR-F01      TO  MSG012-RENNO.
     MOVE  "ERR"        TO  NASI-FLG.
     DISPLAY  MSG012    UPON CONS.
     DISPLAY  MSG-WAKU  UPON CONS.
*****MOVE  4001         TO  PROGRAM-STATUS.
*****EXIT PROGRAM.
*
 NARKANF-NASI-EXIT.
     EXIT.
*↓
****************************************************************
*  該当連携_の状態が、取込依頼済でない場合のメッセージ        *
****************************************************************
 JOUTAI-ERR-SEC       SECTION.
     MOVE  "JOUTAI-ERR-SEC"  TO  S-NAME.
* 状態異常表示
     DISPLAY  MSG-WAKU3 UPON CONS.
     DISPLAY  MSG0311   UPON CONS.
     DISPLAY  MSG0312   UPON CONS.
     MOVE  KTR-F01      TO  MSG032-RENNO.
     MOVE  "ERR"        TO  JOUTAI-FLG.
     DISPLAY  MSG032    UPON CONS.
     DISPLAY  MSG-WAKU3 UPON CONS.
*
 JOUTAI-ERR-EXIT.
     EXIT.
*↑
****************************************************************
*  更新Ｃ２処理 （ＮＧ更新）
****************************************************************
 UPDC2-SEC            SECTION.
     MOVE  "UPDC2-SEC"       TO  S-NAME.
*
     MOVE   KTR-F01         TO  KAN-F01.
*↓
     MOVE   SPACE           TO  ERR-FLG.
*↑

     PERFORM  NARKANF-RD-SEC.
     IF  FG-NARKANF-INV = 1
         PERFORM  NARKANF-NASI-SEC
         GO   TO  UPDC2-EXIT
     END-IF.
*
*↓
     IF  KAN-F04 NOT = 2
         PERFORM JOUTAI-ERR-SEC
         MOVE    1   TO  ERR-FLG
         GO   TO  UPDC2-EXIT
     END-IF.
*↑
     MOVE  KAN-F04          TO  KAN-F05. *> 前回状態FLG
     MOVE  "5"           TO  KAN-F04. *> 状態FLG(取消済にする)
     MOVE  KTR-F98       TO  KAN-F98. *> 返答日付
     MOVE  KTR-F99       TO  KAN-F99. *> 返答時刻
     REWRITE  KAN-REC.

     PERFORM  UPD-SHTDENF-SEC.

     PERFORM  UPD-NARRUIF-SEC.

 UPDC2-EXIT.
     EXIT.
****************************************************************
*  売上伝票ファイル更新                                        *
****************************************************************
 UPD-SHTDENF-SEC            SECTION.
     MOVE  "UPD-SHTDENF-SEC"  TO  S-NAME.

     INITIALIZE  URI-REC.
*↓2012/07/17
**** MOVE  1                TO  URI-F32.  *> 小売連携区分
*↑2012/07/17
     MOVE  KTR-F01          TO  URI-F33.  *> 連携Ｎｏ
     MOVE  LOW-VALUE        TO  FG-SHTDENF-END.

     PERFORM  RD-SHTDENF-SEC.  *> 売上伝票の初期読込み

     PERFORM  UNTIL FG-SHTDENF-END = "END"
       PERFORM  UPD-SHTDENFB-SEC
       PERFORM  RD-SHTDENF-SEC

     END-PERFORM.
 UPD-SHTDENF-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込み　                                  *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
     MOVE  "RD-SHTDENF-SEC" TO  S-NAME.

     IF  FG-SHTDENF-END = LOW-VALUE
         START  SHTDENF  KEY >=
*↓2012/07/17
****            URI-F32  *> 小売連携区分
*↑2012/07/17
                URI-F33  *> 連携Ｎｏ
                URI-F112 *> 納品日
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

*↓2012/07/17
***  IF  URI-F32 = "1" *> 小売連携区分あり
***      CONTINUE
***  ELSE
***      MOVE  "END"        TO  FG-SHTDENF-END
***      GO TO  RD-SHTDENF-EXIT
***  END-IF.
*↑2012/07/17

     IF  URI-F33 = KTR-F01  *> 連携Ｎｏ
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.

     ADD 1   TO  CT-IN.

 RD-SHTDENF-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル更新Ｂ                                    *
****************************************************************
 UPD-SHTDENFB-SEC      SECTION.
     MOVE  "UPD-SHTDENFB-SEC" TO  S-NAME.

     MOVE  SPACE            TO  URI-F33. *> 小売連携Ｎｏ
     REWRITE  URI-REC.
     ADD  1   TO  CT-UP.

 UPD-SHTDENFB-EXIT.
     EXIT.
****************************************************************
*  小売連携累積ファイル更新                                    *
****************************************************************
 UPD-NARRUIF-SEC            SECTION.
     MOVE  "UPD-NARRUIF-SEC"  TO  S-NAME.

     INITIALIZE  RUI-REC.
     MOVE  KTR-F01          TO  RUI-F01.  *> 連携Ｎｏ
     MOVE  LOW-VALUE        TO  FG-NARRUIF-END.

     PERFORM  RD-NARRUIF-SEC.  *> 売上伝票の初期読込み

     PERFORM  UNTIL FG-NARRUIF-END = "END"
       PERFORM  UPD-NARRUIFB-SEC
       PERFORM  RD-NARRUIF-SEC

     END-PERFORM.

 UPD-NRARUIF-EXIT.
     EXIT.
****************************************************************
*  小売連携累積ファイル読込み　                                *
****************************************************************
 RD-NARRUIF-SEC          SECTION.
     MOVE  "RD-NARRUIF-SEC" TO  S-NAME.

     IF  FG-NARRUIF-END = LOW-VALUE
         START  NARRUIF  KEY >=
                RUI-F01  *> 連携Ｎｏ
           INVALID KEY
              MOVE  "END"   TO  FG-NARRUIF-END
              GO TO  RD-NARRUIF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NARRUIF-END

     END-IF.

     READ  NARRUIF
       AT  END
         MOVE  "END"        TO  FG-NARRUIF-END
         GO TO  RD-NARRUIF-EXIT
     END-READ.

     IF  RUI-F01 = KTR-F01  *> 連携Ｎｏ
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-NARRUIF-END
         GO TO  RD-NARRUIF-EXIT
     END-IF.

     ADD 1   TO  CT-IN2.

 RD-NARRUIF-EXIT.
     EXIT.
****************************************************************
*  小売連携累積ファイル更新Ｂ                                  *
****************************************************************
 UPD-NARRUIFB-SEC      SECTION.
     MOVE  "UPD-NARRUIFB-SEC" TO  S-NAME.

     DELETE  NARRUIF.
     ADD  1   TO  CT-DL.

 UPD-NARRUIFB-EXIT.
     EXIT.
****************************************************************
*  作表処理                                                    *
****************************************************************
 PRT-SEC            SECTION.
     MOVE  "PRT-SEC"        TO  S-NAME.
*
 PRT-00.
* 担当者マスタ
**** MOVE  KAN-F02          TO  TAN-F01. *> 部門ＣＤ
     MOVE  KTR-F02          TO  TAN-F01. *> 部門ＣＤ
**** MOVE  KAN-F03          TO  TAN-F02. *> 担当者ＣＤ
     MOVE  KTR-F03          TO  TAN-F02. *> 担当者ＣＤ

     READ  HTANMS
       INVALID
         MOVE  1            TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTANMS-INV
     END-READ.
 PRT-01.
*ＨＥＡＤ
     MOVE  SYS-DATE (1:4)   TO  HD01-Y.
     MOVE  SYS-DATE (5:2)   TO  HD01-M.
     MOVE  SYS-DATE (7:2)   TO  HD01-D.
     ADD  1   TO  CT-PG.
     MOVE  CT-PG            TO  HD01-PG.

     MOVE  WK-TIME (1:2)    TO  HD02-HH.
     MOVE  WK-TIME (3:2)    TO  HD02-MM.
     MOVE  WK-TIME (5:2)    TO  HD02-DD.

* 連携Ｎ-を日本語変換する。
     MOVE  ALL NC"？"       TO  WK-RENNO-N.
     MOVE  KTR-F01          TO  WK-RENNO.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 9
       PERFORM  VARYING  IX2  FROM 1 BY 1
                UNTIL    IX2 > 11
         IF  TB-CVT-SUR (IX2) = WK-RENNOR (IX)
             MOVE  TB-CVT-SU-NR (IX2)  TO WK-RENNO-NR (IX)
             MOVE  11       TO  IX2
         END-IF
       END-PERFORM
     END-PERFORM.
*
     MOVE  WK-RENNO-N       TO  MS01-RENNO.
*
     IF    FG-NARKANF-INV = 1
           MOVE  NC"　　　　　"  TO  MS02-HC-SYUBT
           GO    TO   PRT-02
     END-IF.
*
     IF  KAN-F071 = "1" *> 入力区分
         MOVE  NC"手書"          TO  MS02-HC-SYUBT
     ELSE
         MOVE  NC"オンライン"    TO  MS02-HC-SYUBT
     END-IF.

 PRT-02.
* 結果
     IF  KTR-F05 = "OK"  *> OK
         IF    FG-NARKANF-INV = 1
               MOVE  NC"ＯＫ？"   TO  MS01-2-HC-KEKKA
               MOVE  MEISAI07-MSG TO  MS05-MSG
               MOVE  MEISAI08-MSG TO  MS06-MSG
         ELSE
               MOVE  NC"ＯＫ　"   TO  MS01-2-HC-KEKKA
               MOVE  SPACE        TO  MS05-MSG
               MOVE  SPACE        TO  MS06-MSG
         END-IF
*↓
         IF    ERR-FLG = "1"
               MOVE  NC"ＯＫ？"   TO  MS01-2-HC-KEKKA
               MOVE  MEISAI09-MSG TO  MS05-MSG
               MOVE  MEISAI08-MSG TO  MS06-MSG
         END-IF
*↑
     ELSE                *> NG
         IF    FG-NARKANF-INV = 1
               MOVE  NC"ＮＧ？"   TO  MS01-2-HC-KEKKA
               MOVE  MEISAI07-MSG TO  MS05-MSG
               MOVE  MEISAI08-MSG TO  MS06-MSG
         ELSE
               MOVE  NC"ＮＧ　"   TO  MS01-2-HC-KEKKA
               MOVE  MEISAI05-MSG TO  MS05-MSG
               MOVE  MEISAI06-MSG TO  MS06-MSG
         END-IF
*↓
         IF    ERR-FLG = "1"
               MOVE  NC"ＮＧ？"   TO  MS01-2-HC-KEKKA
               MOVE  MEISAI09-MSG TO  MS05-MSG
               MOVE  MEISAI08-MSG TO  MS06-MSG
         END-IF
*↑
     END-IF.
*
     IF  FG-NARKANF-INV = 1
         MOVE  ALL NC"　"         TO  MS03-SND-KENSU
         GO   TO  PRT-04
     END-IF.
*
 PRT-03.
* データ件数を日本語変換する。
     MOVE  ALL NC"？"       TO  WK-DTKENSU-N.
     MOVE  KAN-F06          TO  WK-DTKENSUR-Z.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 6
       PERFORM  VARYING  IX2  FROM 1 BY 1
                UNTIL    IX2 > 11

         IF  TB-CVT-SUR (IX2) = WK-DTKENSUR (IX)
             MOVE  TB-CVT-SU-NR (IX2)  TO WK-DTKENSU-NR (IX)
             MOVE  11       TO  IX2
         END-IF
       END-PERFORM

     END-PERFORM.

     MOVE  WK-DTKENSU-N     TO  MS03-SND-KENSU.

 PRT-04.
*部門・担当者セット
**** MOVE  KAN-F02          TO  MS04-SND-BUMN.
     MOVE  KTR-F02          TO  MS04-SND-BUMN.
**** MOVE  KAN-F03          TO  MS04-SND-TANT.
     MOVE  KTR-F03          TO  MS04-SND-TANT.

     IF  FG-HTANMS-INV = ZERO
         MOVE  TAN-F03      TO  MS04-SND-TANTNM
     ELSE
         MOVE  ALL NC"＊"   TO  MS04-SND-TANTNM
     END-IF.

 PRT-05.
*プリントアウト
     IF CT-PG > 1
        MOVE  SPACE         TO  PRT-REC
        WRITE  PRT-REC  AFTER PAGE
     END-IF.

     WRITE  PRT-REC  FROM HEAD01      AFTER 3.
     WRITE  PRT-REC  FROM HEAD02      AFTER 1.
     WRITE  PRT-REC  FROM YKSEN       AFTER 1.

     WRITE  PRT-REC  FROM MEISAI01    AFTER 5.
     WRITE  PRT-REC  FROM MEISAI01-2  AFTER 4.
     WRITE  PRT-REC  FROM MEISAI02    AFTER 3.
     WRITE  PRT-REC  FROM MEISAI03    AFTER 3.
     WRITE  PRT-REC  FROM MEISAI04    AFTER 3.
     WRITE  PRT-REC  FROM MEISAI05    AFTER 4.
     WRITE  PRT-REC  FROM MEISAI06    AFTER 1.
     WRITE  PRT-REC  FROM YKSEN       AFTER 2.

 PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*
*ファイル ＣＬＯＳＥ
     CLOSE  NARKTRF.
     CLOSE  SHTDENF.
     CLOSE  NARKANF.
     CLOSE  NARRUIF.
     CLOSE  HTANMS.
     CLOSE  PRTFILE.
     DISPLAY   "**  END   SNA0230B  **"  UPON CONS.
*
*↓
*----IF NASI-FLG = "ERR"
     IF NASI-FLG = "ERR"  OR  JOUTAI-FLG = "ERR"
*↑
        DISPLAY  NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
        DISPLAY  NC"＊　連携_管理テーブルに　＊" UPON CONS
*↓
*-------DISPLAY  NC"＊　存在しない連携_の　　＊" UPON CONS
        DISPLAY  NC"＊　存在しないか、依頼　　＊" UPON CONS
        DISPLAY  NC"＊　していない連携_の　　＊" UPON CONS
*↑
        DISPLAY  NC"＊　取り込み結果通知が　　＊" UPON CONS
        DISPLAY  NC"＊　苗業務システムより　　＊" UPON CONS
        DISPLAY  NC"＊　送信されてきました！　＊" UPON CONS
        DISPLAY  NC"＊　確認してください！！　＊" UPON CONS
        DISPLAY  NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
        MOVE  4001         TO  PROGRAM-STATUS
     END-IF.
*
 END-EXIT.
     EXIT.
*****************<<  SNA0230B   END PROGRAM  >>******************
