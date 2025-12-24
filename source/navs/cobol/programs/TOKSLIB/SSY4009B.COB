****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　セリ売上データ_ユポラベルデータ　*
*    作成日／作成者　　　：　00/02/28                          *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY4009B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<< セリ売上データＦ >>--*
     SELECT      HSERJNL     ASSIGN    TO        DA-01-S-HSERJNL
                             FILE      STATUS    SER-ST.
*----<< ユポラベルデータ >>--*
     SELECT      SERYUPO     ASSIGN              DA-01-S-SERYUPO
                             FILE      STATUS    YUP-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*----<< セリ売上データＦ >>--*
 FD  HSERJNL
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HSERJNL     OF        XFDLIB
     JOINING     SER         AS        PREFIX.
*----<< ユポラベルデータ >>--*
 FD  SERYUPO
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SERYUPO     OF        XFDLIB
     JOINING     YUP         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  SER-ST              PIC  X(02)  VALUE  SPACE.
     03  YUP-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  WK-SURYO            PIC  9(04)  VALUE  ZERO.
     03  WK-YUP-F09          PIC  9(04)  VALUE  ZERO.
     03  WK-SHO              PIC  9(04)  VALUE  ZERO.
     03  WK-AMARI            PIC  9(04)  VALUE  ZERO.
     03  OPEN-FLG            PIC  9(01)  VALUE  ZERO.
 01  WK-CHG.
     03  WK-CHG-1            PIC  N(07)  VALUE  SPACE.
     03  WK-CHG-2            PIC  N(07)  VALUE  SPACE.
*
 01  FILE-ERR.
     03  SER-ERR             PIC  N(10)  VALUE
                   NC"セリ売上データ異常".
     03  YUP-ERR             PIC  N(10)  VALUE
                   NC"ユポラベルＦ異常".
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 SER-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSERJNL.
     DISPLAY     SER-ERR     UPON      STAT.
     DISPLAY     SER-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 YUP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SERYUPO.
     DISPLAY     YUP-ERR     UPON      STAT.
     DISPLAY     YUP-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     OPEN        INPUT       HSERJNL.
*****OPEN        OUTPUT      SERYUPO.
*
     MOVE        ZERO        TO      OPEN-FLG.
*
     PERFORM     HSERJNL-READ-SEC.
*
     IF          END-FLG NOT = 9
                 MOVE    1   TO      OPEN-FLG
                 OPEN  OUTPUT  SERYUPO
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
*ユポラベル項目セット
     MOVE     SPACE          TO       YUP-REC.
     INITIALIZE                       YUP-REC.
*    セリ番号
     MOVE     SER-F01        TO       YUP-F011.
     MOVE     SER-F02        TO       YUP-F012.
*    取引先コード
     MOVE     SER-F11        TO       YUP-F02.
*    相手商品コード
     MOVE     SER-F621       TO       YUP-F03.
*    自社商品コード
     MOVE     SER-F621       TO       YUP-F04.
*    自社品単コード
     MOVE     SER-F622       TO       YUP-F05.
*    ＪＡＮコード
     MOVE     SER-F121       TO       YUP-F06.
*    商品名１
     MOVE     SER-F641       TO       YUP-F071.
*    商品名２
     MOVE     SER-F642       TO       WK-CHG-1.
     MOVE     SER-F643       TO       WK-CHG-2.
     MOVE     WK-CHG         TO       YUP-F072.
*    部門
     MOVE     SER-F08        TO       YUP-F08.
*    金額
     MOVE     SER-F132       TO       YUP-F09.
*ユポラベル発行枚数計算
     MOVE     ZERO           TO        WK-SURYO.
     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
              ADD SER-F163(I) TO       WK-SURYO
     END-PERFORM.
*    枚数
     DIVIDE   WK-SURYO       BY        SER-F66
                             GIVING    WK-SHO
                             REMAINDER WK-AMARI.
     IF       SER-F121(5:3)  =  "505"
              COMPUTE  WK-YUP-F09  =  WK-SHO  *  2
              IF       WK-AMARI NOT = ZERO
                       COMPUTE  WK-YUP-F09  =  WK-YUP-F09  +  2
              END-IF
              MOVE  WK-YUP-F09   TO    YUP-F10
     ELSE
              MOVE  WK-SHO   TO        YUP-F10
     END-IF.
*    ユポラベルデータ作成
     WRITE    YUP-REC.
*    セリ売上データファイル読込み
     PERFORM  HSERJNL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                 項 目 転 送
****************************************************************
 HSERJNL-READ-SEC            SECTION.
*
     READ     HSERJNL    AT    END
              MOVE       9   TO        END-FLG
     END-READ.
*
 HSERJNL-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
     CLOSE    HSERJNL.
*
     IF       OPEN-FLG  =   1
              CLOSE    SERYUPO
     END-IF.
*
 END-EXIT.
     EXIT.
